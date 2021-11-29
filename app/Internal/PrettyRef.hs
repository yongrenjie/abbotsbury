module Internal.PrettyRef
  ( prettify, prettify'
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
prettify'
  :: FilePath           -- ^ Current working directory
  -> Maybe Int          -- ^ Maximum number of displayed authors
  -> Int                -- ^ Currently active reference number
  -> [(Int, Reference)] -- ^ The references
  -> IO [Widget RName]
prettify' cwd authLimit curNum refs = if null refs
  then pure []
  else do
    -- Create the widgets for each reference
    attrsRefs <- mapM (\(i, r) -> makeRefW i cwd authLimit curNum r) refs
    let attrs  = map fst attrsRefs
        refsW1 = map snd attrsRefs
    -- Apply vertical padding to align them
    let (headW2 : refsW2) = applyColPadding (heading : refsW1)
    -- Then generate hBoxes from them and style
    let headW = withAttr "bold" . hBox $ headW2
        refsW3 = map hBox refsW2
        refsW4 = zipWith (foldl' (.) id) attrs refsW3
        refsW =
          viewport ViewportRefs Vertical
            -- .  cached CacheRefs
            .  vBox
            $  fmap (padBottom (Pad 1)) (init refsW4)
            ++ [last refsW4]
    pure [joinBorders . border $ vBox [headW <=> hBorder <=> refsW]]


-- old version for compatibility
prettify :: FilePath -> Maybe Int -> [(Int, Reference)] -> IO Text
prettify cwd authLimit refs = if null refs
                                 then pure ""
                                 else do
    rawColumns <- mkRawColumns cwd authLimit refs
    fieldSizes <- getFieldSizes rawColumns
    let header = mkHeader fieldSizes
        ts     = fmap (convertRawColumnsToText fieldSizes) rawColumns
    pure $ header <> "\n" <> T.intercalate "\n\n" ts


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
   in fmap applyColPaddingOne rows


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
                     then [withAttr "selectedRef", visible]
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
      authorW  = makeSizedWidget $ mkPersonColumn authLimit a
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
      authorW  = makeSizedWidget $ mkPersonColumn authLimit b
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


-- $step1-generate-heads
-- First, we generate the header row. This can really be anything we like.

-- | A constant.
headings :: (Text, Text, Text, Text, Text)
headings = ("   #", "Authors", "Year", "Journal/Publisher", "Title/DOI")

-- | Another constant; basically the lengths of each heading.
headingLengths :: (Int, Int, Int, Int, Int)
headingLengths = headings & each %~ T.length

mkHeader :: FieldSizes -> Text
mkHeader fss = mconcat
  [ setBold (printf (headings ^.. each))
  , "\n"
  , setBold (T.replicate (totalSizes fss) "-")
  ]
 where
  printf :: [Text] -> Text
  printf row =
    foldMap (\(t, fs) -> T.justifyLeft fs ' ' t) (zip row (fss ^.. each))

-- $step2-generate-columns
-- We then need to generate *four* of the columns according to what type of Work
-- the reference contains. The column we *don't* generate is the first one:
-- that's because it contains the refno, and the Work itself doesn't carry that
-- information. It can only be obtained from the (Int, Reference) that is passed
-- into 'prettify'.
--
-- Out of the five columns, it's quite clear that the only one we need to
-- "resize" is actually the *last* column. The other four columns have sizes
-- which are adapted to their largest member, so it makes a lot more sense to
-- create them first before calculating the field sizes for them.

-- | A vertical column of text, which may or may not have been formatted nicely
-- according to some maximal width.
type Column = [Text]

-- | This is an indicator as to whether a Text should be \'cooked\' (i.e. split
-- into chunks of N characters long).
data CookLater = Cook | DontCook deriving (Show, Eq)

-- | This represents the fifth column that hasn't been \'cooked\' yet, i.e.
-- hasn't been split into chunks of N characters yet. We need to keep extra
-- information about whether to \'cook\' each component, because ONLY the title
-- and the tags should be \'cooked\': if we \'cook\' anything with ANSI escape
-- sequences they will be destroyed.
type RawFifthColumn = [(Text, CookLater)]

-- | Generate the *four* last columns (i.e. ignoring the number column).
mkArticleColumns
  :: FilePath   -- ^ Current working directory.
  -> Set Tag    -- ^ Any tags belonging to the Reference.
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> Article
  -> IO (Column, Column, Column, RawFifthColumn)
mkArticleColumns cwd refTags authLimit a = do
  -- This line is the only thing that requires IO. So annoying. I mean, I could
  -- factorise it out, but at the cost of making the function signature worse.
  availText <- mkAvailText cwd [FullText, SI] a
  -- Build up the columns first.
  let authorColumn  = mkPersonColumn authLimit a
      yearColumn    = [T.pack . show $ a ^. year]
      journalColumn = [getShortestJournalName a, getVolInfo a]
      titleColumn =
        [ (a ^. title         , Cook)
        , (a ^. doi           , Cook)
        , (availText          , DontCook)
        , (mkTagText refTags, Cook)
        ]
  pure (authorColumn, yearColumn, journalColumn, titleColumn)

-- | Generate the author column, optionally trimming it to a maximum number of
-- rows.
--
-- It doesn't make sense to trim to a smaller number than 3 (first author +
-- ellipses + last author), so if n < 3 we fall back to 3. (That technically
-- includes zero and negative numbers.)
mkPersonColumn :: Bibliographic w => Maybe Int -> w -> [Text]
mkPersonColumn authLimit w = case authLimit of
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

-- | Does the job for a Book.
mkBookColumns
  :: FilePath   -- ^ Current working directory.
  -> Set Tag    -- ^ Any tags belonging to the Reference.
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> Book
  -> IO (Column, Column, Column, RawFifthColumn)
mkBookColumns cwd refTags authLimit b = do
  availText <- mkAvailText cwd [FullText] b
  -- Build up the columns first.
  let authorColumn  = mkPersonColumn authLimit b
      yearColumn    = [T.pack . show $ b ^. year]
      editionText   = b ^. edition
      editionText2  = if T.null editionText then "" else editionText <> " ed."
      journalColumn = [b ^. publisher, editionText2]
      titleColumn =
        [ (b ^. title         , Cook)
        , (b ^. isbn          , Cook)
        , (availText          , DontCook)
        , (mkTagText refTags, Cook)
        ]
  pure (authorColumn, yearColumn, journalColumn, titleColumn)

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

-- $step3-calculate-field-sizes
-- Calculate the "correct" field sizes, based on the columns we created earlier.

-- | Field sizes for pretty-printing. Note that the \"title\" field contains
-- more than the title: it also has the DOI/ISBN, as well as the availability
-- text. Note also that the first four field sizes include the padding.
type FieldSizes = (Int, Int, Int, Int, Int)

-- | The amount of columns acting as padding between adjacent fields.
fieldPadding :: Int
fieldPadding = 2

-- | Count the total of field sizes.
totalSizes :: FieldSizes -> Int
totalSizes fss = sum $ fss ^.. each

-- | This function basically takes the four columns generated by mkArticleColumn
-- or mkBookColumn and adds on the first one.
mkRawColumns
  :: FilePath
  -> Maybe Int  -- ^ Max number of authors to display. Nothing for unlimited.
  -> [(Int, Reference)]
  -> IO [(Column, Column, Column, Column, RawFifthColumn)]
mkRawColumns cwd authLimit refnosAndRefs = do
  let mkRawColumns1
        :: (Int, Reference)
        -> IO (Column, Column, Column, Column, RawFifthColumn)
      -- This function does it for just one reference.
      mkRawColumns1 (refno, ref) = do
        let refTags = ref ^. tags
        (c2, c3, c4, c5) <- case ref ^. work of
          ArticleWork a -> mkArticleColumns cwd refTags authLimit a
          BookWork    b -> mkBookColumns cwd refTags authLimit b
        let workTypeSymbol = case ref ^. work of
              ArticleWork _ -> "\x1f4dd"
              BookWork    _ -> "\x1f4d8"
        let c1 = [workTypeSymbol <> " " <> (T.pack . show $ refno)]
        pure (c1, c2, c3, c4, c5)
  mapM mkRawColumns1 refnosAndRefs

-- | This converts a RawFifthColumn to a Column proper. If width is set to 0
-- then formats it with an infinite width, i.e. doesn't split up any of the
-- Texts.
cookFifthColumn :: Int -> RawFifthColumn -> Column
cookFifthColumn width | width == 0 = map fst
                      | otherwise  = concatMap cookIndivTexts
 where
  cookIndivTexts :: (Text, CookLater) -> [Text]
  cookIndivTexts (t, c) = if c == Cook then T.chunksOf width t else [t]

-- brittany-disable-next-binding

-- | This function uses the (raw) columns previously generated to calculate the
-- correct field sizes.
getFieldSizes :: [(Column, Column, Column, Column, RawFifthColumn)] -> IO FieldSizes
getFieldSizes rawColumns = do
  -- "Safe" maximum function which defaults to 0.
  let maximum0 :: Foldable t => t Int -> Int
      maximum0 xs = if null xs then 0 else maximum xs
  -- This calculates the width of a column, i.e. the length of the longest
  -- line in it.
  -- >>> columnWidth ["A", "B", "Hello"] = 5
  let columnWidth :: Column -> Int
      columnWidth = maximum0 . fmap T.length
  -- We need to (temporarily) convert the RawFifthColumn into a real Column, so
  -- that we can get its width. This lens expression basically means: for each
  -- tuple, in the fifth entry, perform @cookFifthColumn 0@ to it.
  let tempCooked = rawColumns & each . _5 %~ cookFifthColumn 0
  -- Calculate the maximum size of each of these.
  -- unzip5 tempCooked :: ([Column], [Column], [Column], [Column], [Column])
  -- The lens expression says: to each entry of this 5-tuple, do @maximum0 .
  -- fmap columnWidth@ (which basically the longest width) to it.
  let fss1 = unzip5 tempCooked & each %~ maximum0 . fmap columnWidth
  -- We need to make sure that the first four fields are not any shorter than
  -- their headings. Technically, the fifth field is safe because we will later
  -- enforce a lower limit of 40 characters on it, but we'll do it anyway.
  let fss2 = fss1 & _1 %~ (\l -> max l (headingLengths ^. _1))
                  & _2 %~ (\l -> max l (headingLengths ^. _2))
                  & _3 %~ (\l -> max l (headingLengths ^. _3))
                  & _4 %~ (\l -> max l (headingLengths ^. _4))
                  & _5 %~ (\l -> max l (headingLengths ^. _5))
  -- Then we need to add the padding to the first four fields.
  let fss3 = fss2 & _1 %~ (+ fieldPadding)
                  & _2 %~ (+ fieldPadding)
                  & _3 %~ (+ fieldPadding)
                  & _4 %~ (+ fieldPadding)
  -- The last field needs to be counted very carefully. Essentially, we:
  --   - have a lower limit of 40 (which is the length of the availability
  --     string; if we go below this the ANSI escape sequences go awry)
  --   - have an upper limit of whatever it is now (because that corresponds to
  --     the length of the longest title / tag string / whatever, and there's no
  --     point going beyond that)
  --   - apart from those limits, we ideally want to use up the remaining space
  --     in the terminal.
  termWidth <- maybe 80 TermSize.width <$> TermSize.size
  let lowerLimit = 30
      upperLimit = fss3 ^. _5
      ideal      = termWidth - (fss3 ^. _1 + fss3 ^. _2 + fss3 ^. _3 + fss3 ^. _4)
      result     | ideal < lowerLimit = lowerLimit
                 | ideal > upperLimit = upperLimit
                 | otherwise          = ideal
  let fss = fss3 & _5 .~ result
  pure fss

-- $step4-cook
-- Finally, from the columns we can now generate some real text. We do this by
-- first cooking the fifth column according to the field sizes we calculated
-- earlier. After that, we transpose the columns to rows and print each row
-- according to the field sizes.

-- | This converts the columns into something that we can actually print on
-- screen. Note that the input to this must already be "cooked"!
convertRawColumnsToText
  :: FieldSizes -> (Column, Column, Column, Column, RawFifthColumn) -> Text
convertRawColumnsToText fss rawCols = t
 where
    -- Cook the fifth column according to the field size which was calculated.
  cols            = rawCols & _5 %~ cookFifthColumn (fss ^. _5)
  -- Turn it into a list. That makes life easier.
  colsList        = cols ^.. each
  -- Pad all the columns so that they have the same height.
  maxColumnHeight = maximum . fmap length $ colsList
  padColumn :: Column -> Column
  padColumn c = take maxColumnHeight (c ++ repeat "")
  paddedColumns = fmap padColumn colsList
  -- Transpose it to get the rows instead of columns!!
  rows          = transpose paddedColumns
  -- Convert a row of Texts (one from each column) into a single line of Text.
  -- For some reason safeWctwidth doesn't work on the emoji so we need to
  -- manually hardcode its width.
  getWidth :: Text -> Int
  getWidth t | T.null t = 0
             | T.head t `elem` ['\x1f4dd', '\x1f4d8'] = safeWctwidth t
             | otherwise = safeWctwidth t
  padSpaces :: Text -> Int -> Text
  padSpaces t fs = t <> T.replicate (fs - getWidth t) " "
  printf :: [Text] -> Text
  printf row = foldMap (uncurry padSpaces) (zip row (fss ^.. each))
  -- Then just fmap that over the rows.
  allLines = fmap printf rows
  -- And stick newlines between them. T.unlines gives one extra newline at the
  -- end, which I don't want.
  t        = T.intercalate "\n" allLines
