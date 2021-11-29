module Internal.PrettyRef
  ( prettify
  ) where

import           Abbotsbury.Cite.Helpers.Person
import           Brick.AttrMap
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Brick.Widgets.Table
import           Commands.Shared
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.Foldable                  ( toList )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl'
                                                , transpose
                                                , unzip5
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Graphics.Text.Width            ( safeWctwidth )
import           Internal.Monad
import           Internal.Path
import           Internal.Style                 ( setBold
                                                , setColor
                                                )
import           Internal.Types
import           Lens.Micro.Platform
import           Reference
import qualified System.Console.Terminal.Size  as TermSize
import           System.Directory               ( doesFileExist )
import           Text.Megaparsec                ( eof
                                                , parse
                                                )
import           Text.Wrap                      ( WrapSettings(..)
                                                , defaultWrapSettings
                                                )


data SizedWidget = SizedWidget { widget :: Widget RName
                               , rows   :: Int
                               , cols   :: Int }


makeSizedWidget :: [Text] -> SizedWidget
makeSizedWidget ts =
  SizedWidget (vBox $ fmap txt ts) (length ts) (maximum $ fmap textWidth ts)


type WidgetRow
  = (SizedWidget, SizedWidget, SizedWidget, SizedWidget, Widget RName)


-- Liable to crashing! Use with care
_getColWidth :: Int -> WidgetRow -> Int
_getColWidth i (a, b, c, d, _) = case i of
  1 -> cols a
  2 -> cols b
  3 -> cols c
  4 -> cols d
  _ -> error "This should never happen"


-- | Construct pretty-printed text from a set of references to be displayed on
-- the screen.
prettify
  :: FilePath           -- ^ Current working directory
  -> Maybe Int          -- ^ Maximum number of displayed authors
  -> Int                -- ^ Currently active reference number
  -> [(Int, Reference)] -- ^ The references
  -> IO [Widget RName]
prettify cwd authLimit curNum refs = if null refs
  then pure []
  else do
    -- Create the widgets for each reference
    attrsRefs <- mapM (\(i, r) -> makeRefW i cwd authLimit curNum r) refs
    let attrs  = map fst attrsRefs
        refsW1 = map snd attrsRefs
    -- Apply vertical padding to align them
    let (headW2 : refsW2) = applyColPadding (heading : refsW1)
    -- Then generate hBoxes from them and style
    let headW = withDefAttr "bold" . hBox $ headW2
        refsW3 = map hBox refsW2
        refsW4 = zipWith (foldl' (.) id) attrs refsW3
        refsW =
          withVScrollBars OnRight
            . viewport ViewportRefs Vertical
            .  vBox
            $  fmap (padBottom (Pad 1)) (init refsW4)
            ++ [last refsW4]
    pure [joinBorders . border $ vBox [headW <=> hBorder <=> refsW]]


-- txtWrap but break long words (not sure if it really has any effect)
txtWrap' :: Text -> Widget n
txtWrap' =
  let settings = defaultWrapSettings { breakLongWords = True }
  in  txtWrapWith settings


-- Header row of widgets
heading :: (SizedWidget, SizedWidget, SizedWidget, SizedWidget, Widget RName)
heading =
  let t1 = "  #"
      t2 = "Authors"
      t3 = "Year"
      t4 = "Journal/Publisher"
      t5 = "Title/DOI"
  in  ( makeSizedWidget [t1]
      , makeSizedWidget [t2]
      , makeSizedWidget [t3]
      , makeSizedWidget [t4]
      , txtWrap' t5
      )


-- | Expand the widgets to their appropriate sizes.
applyColPadding
  :: [(SizedWidget, SizedWidget, SizedWidget, SizedWidget, Widget RName)]
  -> [[Widget RName]]
applyColPadding rows =
  let largest1 = maximum $ fmap (_getColWidth 1) rows
      largest2 = maximum $ fmap (_getColWidth 2) rows
      largest3 = maximum $ fmap (_getColWidth 3) rows
      largest4 = maximum $ fmap (_getColWidth 4) rows
      applyColPaddingOne (sw1, sw2, sw3, sw4, w5) =
        [ hLimit (largest1 + 2) . padRight Max $ widget sw1
        , hLimit (largest2 + 2) . padRight Max $ widget sw2
        , hLimit (largest3 + 2) . padRight Max $ widget sw3
        , hLimit (largest4 + 2) . padRight Max $ widget sw4
        , padRight Max w5
        ]
  in  fmap applyColPaddingOne rows


-- Generate a table row from a reference, just delegates appropriately
makeRefW
  :: Int        -- ^ Number of the reference
  -> FilePath   -- ^ Current working directory.
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> Int        -- ^ Number of currently active reference
  -> Reference  -- ^ The reference
  -> IO
       ( [Widget RName -> Widget RName]
       , ( SizedWidget
         , SizedWidget
         , SizedWidget
         , SizedWidget
         , Widget RName
         )
       )
makeRefW i cwd mi curNum ref = do
  let ts = ref ^. tags
  widgets <- case ref ^. work of
    ArticleWork a -> makeArticleW i cwd ts mi a
    BookWork    b -> makeBookW i cwd ts mi b
  let attrs = if i == curNum
                     then [withDefAttr "selectedRef", visible]
                     else []
  pure (attrs, widgets)


-- Heavy lifting for articles
makeArticleW
  :: Int        -- ^ Number of the reference
  -> FilePath   -- ^ Current working directory.
  -> Set Tag    -- ^ Any tags belonging to the Reference.
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> Article    -- ^ The reference
  -> IO
       ( SizedWidget
       , SizedWidget
       , SizedWidget
       , SizedWidget
       , Widget RName
       )
makeArticleW i cwd refTags authLimit a = do
  availText <- mkAvailText cwd [FullText, SI] a
  -- TODO: Double-width emojis also break Brick. "\x1f4dd"
  let numberW  = makeSizedWidget ["A" <> " " <> (T.pack . show $ i)]
      authorW  = makeSizedWidget $ formatAuthors authLimit a
      yearW    = makeSizedWidget [T.pack . show $ a ^. year]
      journalW = makeSizedWidget [getShortestJournalName a, getVolInfo a]
      titleW   = vBox
        $ fmap txtWrap' [a ^. title, a ^. doi, availText, mkTagText refTags]
  pure (numberW, authorW, yearW, journalW, titleW)


-- Heavy lifting for books
makeBookW
  :: Int        -- ^ Number of the reference
  -> FilePath   -- ^ Current working directory.
  -> Set Tag    -- ^ Any tags belonging to the Reference.
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> Book       -- ^ The reference
  -> IO (SizedWidget, SizedWidget, SizedWidget, SizedWidget, Widget RName)
makeBookW i cwd refTags authLimit b = do
  availText <- mkAvailText cwd [FullText] b
  -- TODO: Double-width emojis also break Brick. "\x1f4d8"
  let numberW  = makeSizedWidget ["B" <> " " <> (T.pack . show $ i)]
      authorW  = makeSizedWidget $ formatAuthors authLimit b
      yearW    = makeSizedWidget [T.pack . show $ b ^. year]
      editionText   = b ^. edition
      editionText2  = if T.null editionText then "" else editionText <> " ed."
      editionW = makeSizedWidget [b ^. publisher, editionText2]
      titleW = vBox $ fmap txtWrap'
        [ b ^. title
        , b ^. isbn
        , availText
        , mkTagText refTags
        ]
  pure (numberW, authorW, yearW, editionW, titleW)


-- | Generate the author column, optionally trimming it to a maximum number of
-- rows.
--
-- It doesn't make sense to trim to a smaller number than 3 (first author +
-- ellipses + last author), so if n < 3 we fall back to 3. (That technically
-- includes zero and negative numbers.)
formatAuthors :: Bibliographic w => Maybe Int -> w -> [Text]
formatAuthors authLimit w = case authLimit of
  Just n | n >= 3    -> trimIfOver n allContribs
         | otherwise -> trimIfOver 3 allContribs
  Nothing -> allContribs
 where
  allContribs = formatPersonForList <$> getContributors w
  trimIfOver :: Int -> [Text] -> [Text]
  trimIfOver n ts =
    if n >= 4 && length ts > n then take (n - 2) ts ++ ["...", last ts] else ts


-- | In the 'list' command, we display authors as (e.g.) JRJ Yong.
formatPersonForList :: Person -> Text
formatPersonForList auth =
  let fam = auth ^. family
  in  case auth ^. given of
        Nothing -> fam
        Just gvn ->
          (joinInitialsWith "" "" "" . getInitials $ gvn) <> " " <> fam


-- | Get availability string for a reference.
mkAvailText :: Bibliographic x => FilePath -> [PdfType] -> x -> IO Text
mkAvailText cwd types w = do
  texts <- forM types $ \t -> do
    avail <- doesFileExist $ getPdfPath t cwd w
    -- TODO: ANSI escape sequences break Brick, find a workaround
    -- let symbol = if avail
    --       then setColor "seagreen" "\x2714"
    --       else setColor "crimson" "\x2718"
    let symbol = if avail
          then "\x2714"
          else "\x2718"
    pure $ symbol <> " " <> showPdfType t
  pure $ T.unwords texts


-- | Produce as short a journal name as possible, by removing special characters
-- (only alphanumeric characters and spaces are retained), as well as using some
-- acronyms such as "NMR". This function is only really used for the list command
-- so we can keep it here.
getShortestJournalName :: Article -> Text
getShortestJournalName =
  replaceAcronyms
    . T.strip
    . T.filter (\c -> isAlphaNum c || isSpace c)
    . (^. journalShort)
 where
  acronyms =
    [ ("Nucl Magn Reson"     , "NMR")
    , ("Oxford University"   , "OU")
    , ("Cambridge University", "CU")
    ]
  replaceAcronyms startText =
    foldl' (flip (uncurry T.replace)) startText acronyms


-- | Produce information about the volume, issue, and page numbers of a reference.
-- This output is only meant for list printing, hence is placed here. It's a slight reworking of one
-- of the functions in Abbotsbury.Cite.Styles.ACS (which isn't exported) (but anyway we want
-- slightly different output).
getVolInfo :: Article -> Text
getVolInfo a = T.intercalate ", "
  $ filter (not . T.null) [theVolInfo, thePages]
 where
  thePages   = displayPages (a ^. pages)
  theVolInfo = case (a ^. volume, a ^. issue) of
    (""    , ""    ) -> ""
    (""    , theIss) -> "No. " <> theIss
    (theVol, ""    ) -> theVol
    (theVol, theIss) -> theVol <> " (" <> theIss <> ")"


-- | Generate a one-liner describing the tags.
mkTagText :: Set Text -> Text
mkTagText tagSet = case ts of
  [] -> ""
  _  -> "[" <> T.intercalate ", " ts <> "]"
  where ts = toList tagSet
