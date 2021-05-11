module Commands.List (runList) where

import           Abbotsbury.Cite.Helpers.Author
import           Commands.Shared
import           Path                           ( PDFType(..)
                                                , getPDFPath
                                                )
import           Reference
import           Style                          ( setBold
                                                , setColor
                                                )
import           Control.Monad.Except
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl'
                                                , zip5
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           System.Console.ANSI            ( getTerminalSize )
import           System.Directory               ( doesFileExist )
import           Text.Megaparsec                ( eof
                                                , errorBundlePretty
                                                , parse
                                                )


prefix :: Text
prefix = "list: "


throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ prefix <> e


runList :: Args -> CmdInput -> CmdOutput
runList args input = do
  -- If no refs present, error immediately
  when (IM.null $ refsin input) (throwErrorWithPrefix "no references found")
  let cwd     = cwdin input
      refs    = refsin input
      numRefs = fst $ IM.findMax refs
  -- Parse arguments
  refnos <- parseInCommand (pRefnos <* eof) args prefix
  -- First, check for any refnos that don't exist
  let badRefnos = refnos IS.\\ IM.keysSet refs
  unless
    (IS.null badRefnos)
    (throwErrorWithPrefix
      ("reference(s) " <> intercalateCommas badRefnos <> " not found")
    )
  -- If we reached here, everything is good
  let refnosToPrint =
        if IS.null refnos then IS.fromList [1 .. numRefs] else refnos
  liftIO $ TIO.putStrLn =<< prettifyRefs cwd
                                         (IM.restrictKeys refs refnosToPrint)
  pure $ SCmdOutput refs (Just refnosToPrint)


-- | The field sizes for pretty-printing. Note that the title field is also
-- responsible for the DOI, as well as the availability columns.
-- Note that the first four field sizes include the padding.
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


-- | Calculate the correct field sizes based on the terminal size.
getFieldSizes :: IntMap Reference -> IO FieldSizes
getFieldSizes refs = do
  -- Number field.
  let maxRef   = fst $ IM.findMax refs
      numberF' = length (show maxRef) + fieldPadding
  -- Author field.
  let
    getMaxAuthorLength :: Reference -> Int
    getMaxAuthorLength =
      maximum . fmap (T.length . formatAuthorForList) . (^. (work . authors))
    authorF' = fieldPadding + maximum (map getMaxAuthorLength (IM.elems refs))
  -- Year field.
  let yearF' = fieldPadding + 4
  -- Journal field.
  let longestJName =
        maximum (map (T.length . getShortestJournalName) (IM.elems refs))
      longestJInfo = maximum (map (T.length . getVolInfo) (IM.elems refs))
      journalF'    = fieldPadding + max longestJName longestJInfo
  -- Title field. Our first guess is to just use up the remaining space.
  Just (_, ncols) <- getTerminalSize
  let titleF1 = ncols - numberF' - authorF' - yearF' - journalF'
  -- We enforce an upper limit, which is the longest title / DOI / availability string.
      longestTitle =
        maximum $ map (T.length . (^. (work . title))) (IM.elems refs)
      longestDOI  = maximum $ map (T.length . (^. (work . doi))) (IM.elems refs)
      availLength = 40
      upperLimit  = maximum [longestTitle, longestDOI, availLength]
      titleF2     = min titleF1 upperLimit
  -- We also enforce a lower limit, which is the availability string itself: or else
  -- the ANSI escape sequences tend to get messed up.
  let titleF' = max availLength titleF2
  pure $ FieldSizes numberF' authorF' yearF' journalF' titleF'


-- | Construct pretty-printed text from a set of references to be displayed on the screen.
-- The output of this text can be directly passed to (the Text version of) putStrLn.
prettifyRefs
  :: FilePath -- ^ Current working directory.
  -> IntMap Reference -- ^ The references to be printed.
  -> IO Text
prettifyRefs cwd refs = if IM.null refs
  then pure ""
  else do
    fss <- liftIO $ getFieldSizes refs
    let headText = prettifyHead fss
    refsText <- liftIO $ mapM (prettifyOneRef fss cwd) (IM.assocs refs)
    let text = headText <> "\n" <> T.intercalate "\n\n" refsText <> "\n"   -- extra blank line looks nice.
    pure text


-- | Generate a pretty header for the reference list. This function should only ever be called by
-- prettifyRefs.
prettifyHead
  :: FieldSizes -- ^ The sizes of each field in the output. Must be precalculated using getFieldSizes.
  -> Text
prettifyHead fss =
  setBold (formatLine fss ("#", "Authors", "Year", "Journal", "Title and DOI"))
    <> "\n"
    <> setBold (T.replicate (totalSizes fss) "-")


-- | Generate a pretty header for one single reference. This function should only ever be called by
-- prettifyRefs.
prettifyOneRef
  :: FieldSizes -- ^ The sizes of each field in the output. Must be precalculated using getFieldSizes.
  -> FilePath -- ^ Current working directory.
  -> (Int, Reference) -- ^ The refno and the reference to be printed. Most easily generated using IM.assocs.
  -> IO Text
prettifyOneRef fss cwd (index, ref) = do
  let tagString = case ref ^. tags of
        [] -> ""
        ts -> "[" <> T.intercalate ", " ts <> "]"
  -- This line is the only thing that requires IO. So annoying.
  availString <- getAvailString cwd ref
  -- Build up the columns first.
  let numberColumn = [T.pack $ show index]
      authorColumn1 =
        NE.toList $ fmap formatAuthorForList (ref ^. (work . authors))
      authorColumn = if length authorColumn1 <= 5
        then authorColumn1
                        -- Inefficient but probably not important.
        else take 3 authorColumn1 ++ ["...", last authorColumn1]
      yearColumn    = [T.pack . show $ ref ^. (work . year)]
      journalColumn = [getShortestJournalName ref, getVolInfo ref]
      titleColumn =
        T.chunksOf (titleF fss) (ref ^. (work . title))
          ++ [ref ^. (work . doi)]
          ++ [availString]
          ++ T.chunksOf (titleF fss) tagString
      -- Clone of Python's itertools.zip_longest(fillvalue="").
      zipLongest5
        :: [Text]  -- unpadded number column
        -> [Text]  -- unpadded author column
        -> [Text]  -- unpadded year column
        -> [Text]  -- unpadded journal column
        -> [Text]  -- unpadded title Column
        -> [(Text, Text, Text, Text, Text)]
      zipLongest5 as bs cs ds es =
        let maxLen = maximum $ map length [as, bs, cs, ds, es]
            pad xs = xs ++ repeat ""
        in  take maxLen (zip5 (pad as) (pad bs) (pad cs) (pad ds) (pad es))
  pure . T.intercalate "\n" . map (formatLine fss) $ zipLongest5 numberColumn
                                                                 authorColumn
                                                                 yearColumn
                                                                 journalColumn
                                                                 titleColumn


-- | In the 'list' command, we display authors as (e.g.) JRJ Yong.
formatAuthorForList :: Author -> Text
formatAuthorForList auth =
  let fam = auth ^. family
  in  case auth ^. given of
        Nothing  -> fam
        Just gvn -> (joinInitialsWith "" "" "" . getInitials $ gvn) <> " " <> fam


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
  pure $ mconcat [makeSymbol fullTextAvail, " pdf ", makeSymbol siAvail, " si"]
