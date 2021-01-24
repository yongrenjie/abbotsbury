module Abbot.Commands.List
  ( module Abbot.Commands.List
  ) where

import           Abbot.Commands.Shared
import           Abbot.Reference
import           Abbot.Style                    ( setBold )

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           Text.Megaparsec
import           System.Console.Terminal.Size  as TS


runList :: ReplArgs -> IntMap Reference -> CmdOutput
runList args refs = if IM.null refs
  then cmdErrS "list: no references found"
  else
    let parsedArgs = parse pRefnos "" args
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
                result <- prettyFormatRefs (IM.restrictKeys refs refnosToPrint)
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
  let maxRef    = fst $ IM.findMax refs
      numberF'  = length (show maxRef) + fieldPadding
  -- TODO Author field.
  let authorF'  = 7 + fieldPadding   -- NOT CORRECT
  -- Year field.
  let yearF'    = 4 + fieldPadding
  -- Journal field.
  let journalF' = fieldPadding + maximum (map (T.length . getShortestJournalName) (IM.elems refs))
  -- Title field. The lower limit of 40 is to make sure that ANSI escape sequences
  -- in the availability row (printed under the title) don't get messed up.
  Just (Window _ (ncols :: Int)) <- TS.size  -- errors if output is not to a terminal
  let titleF' = max 40 (ncols - numberF' - authorF' - yearF' - journalF')
  pure $ FieldSizes numberF' authorF' yearF' journalF' titleF'

-- | Prettify an IntMap of references.
prettyFormatRefs :: IntMap Reference -> IO (Either Text Text)
prettyFormatRefs refs = if IM.null refs
  then pure $ Left "no references found"
  else do
    fss <- getFieldSizes refs
    let text = prettyFormatHead fss
          <> T.intercalate "\n\n" (map (printRef fss) (IM.assocs refs))
    pure $ Right text

-- | Generate a pretty header for the reference list.
prettyFormatHead :: FieldSizes -> Text
prettyFormatHead fss =
  setBold
      (mconcat
        [ T.justifyLeft (numberF fss) ' ' "#"
        , T.justifyLeft (authorF fss) ' ' "Authors"
        , T.justifyLeft (yearF fss) ' ' "Year"
        , T.justifyLeft (journalF fss) ' ' "Journal"
        , T.justifyLeft (titleF fss) ' ' "Title and DOI"
        ]
      )
    <> "\n"
    <> T.replicate (totalSizes fss) "-"

-- | Generate pretty output for one particular reference in an IntMap.
printRef :: FieldSizes -> (Int, Reference) -> Text
printRef fss (index, ref) = mconcat
  [ T.justifyLeft (numberF fss)  ' ' (T.pack $ show index)
  , T.justifyLeft (authorF fss)  ' ' ".author."
  , T.justifyLeft (yearF fss)    ' ' (T.pack . show $ ref ^. year)
  , T.justifyLeft (journalF fss) ' ' (getShortestJournalName ref)
  , T.justifyLeft (titleF fss)   ' ' (ref ^. title)
  ]
