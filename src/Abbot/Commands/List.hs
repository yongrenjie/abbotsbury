module Abbot.Commands.List
  ( module Abbot.Commands.List
  ) where

import           Abbot.Commands.Shared
import           Abbot.Path                     ( PDFType(..)
                                                , getPDFPath
                                                )
import           Abbot.Reference
import           Abbot.Style                    ( setBold
                                                , setColor
                                                )

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl'
                                                , zip5
                                                )
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

runList :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runList args cwd refs = if IM.null refs
  then cmdErr "list: no references found"
  else
    let parsedArgs = parse (pRefnos <* eof) "" args
        numRefs    = fst $ IM.findMax refs
    in  case parsedArgs of
          Left bundle -> cmdErrS ("list: " ++ errorBundlePretty bundle)  -- parse error
          Right refnos ->
            -- Some refnos were specified. First, check for out of bounds.
            case IS.lookupGT numRefs refnos <|> IS.lookupLT 1 refnos of
              Just x ->
                cmdErrS ("list: reference " <> show x <> " is out of bounds")
              -- No out of bounds. If refnos is empty, then print all references, otherwise
              -- print whatever was specified.
              Nothing -> do
                let
                  refnosToPrint = if IS.null refnos
                    then IS.fromList [1 .. numRefs]
                    else refnos
                result <- prettyFormatRefs cwd (IM.restrictKeys refs refnosToPrint)
                case result of
                  Left  errMsg        -> cmdErr errMsg
                  Right formattedRefs -> do
                    TIO.putStrLn formattedRefs
                    pure $ Right (refs, Just refnos)


-- | The field sizes for pretty-printing. Note that the title field is also
-- responsible for the DOI, as well as the availability columns.
-- Note that the first four field sizes include the padding.
data FieldSizes = FieldSizes { numberF  :: Int
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
      maximum . map (T.length . formatAuthor ListCmd) . (^. authors)
    authorF' = fieldPadding + maximum (map getMaxAuthorLength (IM.elems refs))
  -- Year field.
  let yearF' = fieldPadding + 4
  -- Journal field.
  let longestJName = maximum (map (T.length . getShortestJournalName) (IM.elems refs))
      longestJInfo = maximum (map (T.length . getVolInfo) (IM.elems refs))
      journalF' = fieldPadding + max longestJName longestJInfo
  -- Title field. Our first guess is to just use up the remaining space.
  Just (_, ncols) <- getTerminalSize
  let titleF1      = ncols - numberF' - authorF' - yearF' - journalF'
  -- We enforce an upper limit, which is the longest title / DOI / availability string.
      longestTitle = maximum $ map (T.length . (^. title)) (IM.elems refs)
      longestDOI   = maximum $ map (T.length . (^. doi)) (IM.elems refs)
      availLength  = 40
      upperLimit   = maximum [longestTitle, longestDOI, availLength]
      titleF2      = min titleF1 upperLimit
  -- We also enforce a lower limit, which is the availability string itself: or else
  -- the ANSI escape sequences tend to get messed up.
  let titleF' = max availLength titleF2
  pure $ FieldSizes numberF' authorF' yearF' journalF' titleF'

-- | Prettify an IntMap of references.
prettyFormatRefs :: FilePath -> IntMap Reference -> IO (Either Text Text)
prettyFormatRefs cwd refs = if IM.null refs
  then pure $ Left "no references found"
  else do
    fss <- getFieldSizes refs
    let headText = prettyFormatHead fss
    refsText <- mapM (printRef fss cwd) (IM.assocs refs)
    let text = headText
          <> "\n"
          <> T.intercalate "\n\n" refsText
          <> "\n"   -- extra blank line looks nice.
    pure $ Right text

-- | Generate a pretty header for the reference list.
prettyFormatHead :: FieldSizes -> Text
prettyFormatHead fss =
  setBold (formatLine fss ("#", "Authors", "Year", "Journal", "Title and DOI"))
    <> "\n"
    <> setBold (T.replicate (totalSizes fss) "-")

-- | Generate pretty output for one particular reference in an IntMap.
printRef :: FieldSizes -> FilePath -> (Int, Reference) -> IO Text
printRef fss cwd (index, ref) = do
  -- This line is the only thing that requires IO. So annoying.
  availString <- getAvailString cwd ref
  -- Build up the columns first.
  let numberColumn  = [T.pack $ show index]
      authorColumn1 = map (formatAuthor ListCmd) (ref ^. authors)
      authorColumn  = if length authorColumn1 <= 5
                         then authorColumn1
                         -- Inefficient but probably not important.
                         else take 3 authorColumn1 ++ ["...", last authorColumn1]
      yearColumn    = [T.pack . show $ ref ^. year]
      journalColumn = [getShortestJournalName ref, getVolInfo ref]
      titleColumn =
        T.chunksOf (titleF fss) (ref ^. title) ++ [ref ^. doi, availString]
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

-- | Utility function to generate one line of output according to the field sizes
-- and the text to be placed there.
formatLine :: FieldSizes -> (Text, Text, Text, Text, Text) -> Text
formatLine fss texts = mconcat
  [ T.justifyLeft (numberF fss)  ' ' (texts ^. _1)
  , T.justifyLeft (authorF fss)  ' ' (texts ^. _2)
  , T.justifyLeft (yearF fss)    ' ' (texts ^. _3)
  , T.justifyLeft (journalF fss) ' ' (texts ^. _4)
  , T.justifyLeft (titleF fss)   ' ' (texts ^. _5)
  ]

-- | Produce as short a journal name as possible, by removing special characters
-- (only alphanumeric characters and spaces are retained), as well as using some
-- acronyms such as "NMR". This function is only really used for the list command
-- so we can keep it here.
getShortestJournalName :: Reference -> Text
getShortestJournalName =
  replaceAcronyms
    . T.strip
    . T.filter ((||) <$> isAlphaNum <*> isSpace)
    . view journalShort
 where
  acronyms = [("Nucl Magn Reson", "NMR")]
  replaceAcronyms startText =
    foldl' (flip (uncurry T.replace)) startText acronyms


-- | Produce information about the volume, issue, and page numbers of a reference.
-- This output is only meant for list printing, hence is placed here.
getVolInfo :: Reference -> Text
getVolInfo ref =
  let theVolume = ref ^. volume
      theIssue  = ref ^. issue
      thePages  = ref ^. pages
  in  if T.null theIssue
        then mconcat [theVolume, ", ", thePages]
        else mconcat [theVolume, " (", theIssue, "), ", thePages]


-- | Get availability string for a reference.
getAvailString :: FilePath -> Reference -> IO Text
getAvailString cwd ref = do
  fullTextAvail <- doesFileExist $ getPDFPath FullText cwd ref
  siAvail <- doesFileExist $ getPDFPath SI cwd ref
  let makeSymbol :: Bool -> Text
      makeSymbol x = if x then setColor "seagreen" "\x2714" else setColor "crimson" "\x2718"
  pure $ mconcat [makeSymbol fullTextAvail, " pdf ", makeSymbol siAvail, " si"]
