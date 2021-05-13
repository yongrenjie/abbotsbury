module Internal.PrettyRef
  ( prettify
  ) where

import           Abbotsbury.Cite.Helpers.Author
import           Commands.Shared
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl'
                                                , maximumBy
                                                , zip5
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Ord                       ( comparing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Internal.Path                  ( PDFType(..)
                                                , getPDFPath
                                                )
import           Internal.Style                 ( setBold
                                                , setColor
                                                )
import           Lens.Micro.Platform
import           Reference
import           System.Console.ANSI            ( getTerminalSize )
import           System.Directory               ( doesFileExist )
import           Text.Megaparsec                ( eof
                                                , parse
                                                )

-- | Construct pretty-printed text from a set of references to be displayed on
-- the screen. The output of this text can be directly passed to
-- 'Data.Text.IO.putStrLn'.
prettify :: FilePath -> [(Int, Reference)] -> IO Text
prettify cwd refs = if null refs
  then pure ""
  else do
    fieldSizes <- liftIO $ getFieldSizes refs
    let headText = prettifyHead fieldSizes
    refsText <- liftIO $ mapM (prettifyOneRef fieldSizes cwd) refs
    -- Extra blank line looks nice.
    pure $ headText <> "\n" <> T.intercalate "\n\n" refsText <> "\n"

-- | Get the field sizes for pretty-printing. Note that the title field is also
-- responsible for the DOI, as well as the availability columns. Note that the
-- first four field sizes include the padding.
data FieldSizes = FieldSizes
  { numberF  :: Int
  , authorF  :: Int
  , yearF    :: Int
  , journalF :: Int
  , titleF   :: Int
  }

-- | The amount of columns acting as padding between adjacent fields.
fieldPadding :: Int
fieldPadding = 2

-- | Count the total of field sizes.
totalSizes :: FieldSizes -> Int
totalSizes fss = sum $ map ($ fss) [numberF, authorF, yearF, journalF, titleF]

-- | A constant.
headings :: (Text, Text, Text, Text, Text)
headings = ("#", "Authors", "Year", "Journal/Publisher", "Title/DOI")

-- | Calculate the correct field sizes based on the terminal size.
getFieldSizes :: [(Int, Reference)] -> IO FieldSizes
getFieldSizes refnosAndRefs = do
  let (numberH, authorH, yearH, journalH, titleH) = headings
  -- Useful things.
  let refnos = map fst refnosAndRefs
      refs   = map snd refnosAndRefs
      longestBy :: (Reference -> Text) -> Int
      longestBy key = maximum $ fmap (T.length . key) refs
  -- Number field.
  let maxRefno = maximum refnos
      numberF' = fieldPadding + length (show maxRefno)
  -- Author field.
  let getLongestAuthor :: Reference -> Text
      getLongestAuthor ref = maximumBy
        (comparing T.length)
        (formatAuthorForList <$> ref ^. (work . authors))
      authorF' = fieldPadding + longestBy getLongestAuthor
  -- Year field.
  let yearF' = fieldPadding + 4
  -- Journal field.
  let longestJName = longestBy getShortestJournalName
      longestJInfo = longestBy getVolInfo
      journalF' =
        fieldPadding + maximum [longestJName, longestJInfo, T.length journalH]
  -- Title field. Our first guess is to just use up the remaining space.
  Just (_, ncols) <- getTerminalSize
  let titleF1      = ncols - numberF' - authorF' - yearF' - journalF'
      -- We enforce an upper limit, which is the longest title / DOI /
      -- availability string.
      longestTitle = longestBy (^. (work . title))
      longestDOI   = longestBy (^. (work . doi))
      availLength  = 40
      upperLimit   = maximum [longestTitle, longestDOI, availLength]
      titleF2      = min titleF1 upperLimit
  -- We also enforce a lower limit, which is the availability string itself: or else
  -- the ANSI escape sequences tend to get messed up.
  let titleF' = max availLength titleF2
  pure $ FieldSizes numberF' authorF' yearF' journalF' titleF'

-- | Generate a pretty header for the reference list. This function should only
-- ever be called by 'prettify'.
prettifyHead :: FieldSizes -> Text
prettifyHead fss = setBold (formatLine fss headings) <> "\n" <> setBold
  (T.replicate (totalSizes fss) "-")

-- | Generate a pretty header for one single reference. This function should
-- only ever be called by 'prettify'.
prettifyOneRef
  :: FieldSizes
  -> FilePath
  -> (Int, Reference)  -- ^ Refno and ref, generated using 'Data.IntMap.assocs'
  -> IO Text
prettifyOneRef fss fp (i, ref) = do
  columns <- case ref ^. (work . workType) of
    JournalArticle -> makeArticleColumns fss fp (i, ref)
    Book           -> makeBookColumns fss fp (i, ref)
    _              -> pure (["unsupported"], [], [], [], [])
  pure . T.intercalate "\n" . map (formatLine fss) $ zipLongest5 columns

-- | Does the job for an article.
makeArticleColumns
  :: FieldSizes
  -> FilePath
  -> (Int, Reference)  -- ^ Refno and ref, generated using 'Data.IntMap.assocs'
  -> IO ([Text], [Text], [Text], [Text], [Text])
makeArticleColumns fss cwd (index, ref) = do
  -- This line is the only thing that requires IO. So annoying. I mean, I could
  -- factorise it out, but at the cost of making the function signature worse.
  availString <- getAvailString cwd ref
  -- Build up the columns first.
  let numberColumn  = [T.pack $ show index]
      authorColumn  = getAuthorColumn 5 ref
      yearColumn    = [T.pack . show $ ref ^. (work . year)]
      journalColumn = [getShortestJournalName ref, getVolInfo ref]
      titleColumn =
        T.chunksOf (titleF fss) (ref ^. (work . title))
          ++ [ref ^. (work . doi)]
          ++ [availString]
          ++ T.chunksOf (titleF fss) (getTagString ref)
  pure (numberColumn, authorColumn, yearColumn, journalColumn, titleColumn)

-- | Does the job for a Book.
makeBookColumns
  :: FieldSizes
  -> FilePath
  -> (Int, Reference)  -- ^ Refno and ref, generated using 'Data.IntMap.assocs'
  -> IO ([Text], [Text], [Text], [Text], [Text])
makeBookColumns fss cwd (index, ref) = do
  -- This line is the only thing that requires IO. So annoying. I mean, I could
  -- factorise it out, but at the cost of making the function signature worse.
  availString <- getAvailString cwd ref
  -- Build up the columns first.
  let numberColumn  = [T.pack $ show index]
      authorColumn  = getAuthorColumn 5 ref
      yearColumn    = [T.pack . show $ ref ^. (work . year)]
      journalColumn = ["(book)", ref ^. (work . publisher)]
      titleColumn   = T.chunksOf (titleF fss) (ref ^. (work . title))
          ++ [availString]
          ++ T.chunksOf (titleF fss) (getTagString ref)
  pure (numberColumn, authorColumn, yearColumn, journalColumn, titleColumn)

-- | In the 'list' command, we display authors as (e.g.) JRJ Yong.
formatAuthorForList :: Author -> Text
formatAuthorForList auth =
  let fam = auth ^. family
  in  case auth ^. given of
        Nothing -> fam
        Just gvn ->
          (joinInitialsWith "" "" "" . getInitials $ gvn) <> " " <> fam

-- | Generate the author column. We don't need to worry about field sizes
-- because this has been sorted out beforehand.
getAuthorColumn :: Int -> Reference -> [Text]
getAuthorColumn n ref = trimIfOverN fullAuthors
 where
  fullAuthors = formatAuthorForList <$> ref ^. (work . authors) ^.. each
  -- It doesn't make sense to trim if n is smaller than 4.
  trimIfOverN :: [Text] -> [Text]
  trimIfOverN ts =
    if n >= 4 && length ts > n then take (n - 2) ts ++ ["...", last ts] else ts

-- | Utility function to generate one line of output according to the field sizes
-- and the text to be placed there.
formatLine :: FieldSizes -> (Text, Text, Text, Text, Text) -> Text
formatLine fss texts = mconcat
  [ T.justifyLeft (numberF fss) ' ' (texts ^. _1)
  , T.justifyLeft (authorF fss) ' ' (texts ^. _2)
  , T.justifyLeft (yearF fss) ' ' (texts ^. _3)
  , T.justifyLeft (journalF fss) ' ' (texts ^. _4)
  , T.justifyLeft (titleF fss) ' ' (texts ^. _5)
  ]

-- | Generate a one-liner describing the tags.
getTagString :: Reference -> Text
getTagString ref = case ref ^. tags of
  [] -> ""
  ts -> "[" <> T.intercalate ", " ts <> "]"

-- | Produce as short a journal name as possible, by removing special characters
-- (only alphanumeric characters and spaces are retained), as well as using some
-- acronyms such as "NMR". This function is only really used for the list command
-- so we can keep it here.
getShortestJournalName :: Reference -> Text
getShortestJournalName =
  replaceAcronyms . T.strip . T.filter ((||) <$> isAlphaNum <*> isSpace) . view
    (work . journalShort)
 where
  acronyms = [("Nucl Magn Reson", "NMR")]
  replaceAcronyms startText =
    foldl' (flip (uncurry T.replace)) startText acronyms

-- | Produce information about the volume, issue, and page numbers of a reference.
-- This output is only meant for list printing, hence is placed here. It's a slight reworking of one
-- of the functions in Abbotsbury.Cite.Styles.ACS (which isn't exported) (but anyway we want
-- slightly different output).
getVolInfo :: Reference -> Text
getVolInfo ref = T.intercalate ", "
  $ filter (not . T.null) [theYear, theVolInfo, thePages]
 where
  theYear  = T.pack (show (ref ^. work . year))
  thePages = case (ref ^. work . pages, ref ^. work . articleNumber) of
    ("", "") -> ""
    ("", aN) -> "No. " <> aN <> ""
    (pg, _ ) -> pg <> ""
  theVolInfo = case (ref ^. work . volume, ref ^. work . issue) of
    (""    , ""    ) -> ""
    (""    , theIss) -> "No. " <> theIss
    (theVol, ""    ) -> theVol
    (theVol, theIss) -> theVol <> " (" <> theIss <> ")"

-- | Get availability string for a reference.
getAvailString :: FilePath -> Reference -> IO Text
getAvailString cwd ref = do
  fullTextAvail <- doesFileExist $ getPDFPath FullText cwd ref
  siAvail       <- doesFileExist $ getPDFPath SI cwd ref
  let makeSymbol :: Bool -> Text
      makeSymbol x = if x
        then setColor "seagreen" "\x2714"
        else setColor "crimson" "\x2718"
  pure $ case ref ^. (work . workType) of
    JournalArticle ->
      mconcat [makeSymbol fullTextAvail, " pdf ", makeSymbol siAvail, " si"]
    Book -> mconcat [makeSymbol fullTextAvail, " pdf"]
    _    -> ""

-- | Clone of Python's itertools.zip_longest(fillvalue=""). It's even uncurried.
-- (That's purely for the sake of convenience, though.)
zipLongest5
  :: ([Text], [Text], [Text], [Text], [Text])
  -> [(Text, Text, Text, Text, Text)]
zipLongest5 (as, bs, cs, ds, es) =
  let maxLen = maximum $ map length [as, bs, cs, ds, es]
      pad xs = xs ++ repeat ""
  in  take maxLen (zip5 (pad as) (pad bs) (pad cs) (pad ds) (pad es))
