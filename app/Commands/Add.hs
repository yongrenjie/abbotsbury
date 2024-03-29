{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Commands.Add
  ( runAdd
  ) where

import           Abbotsbury
import           Commands.Shared
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.Either                    ( partitionEithers )
import qualified Data.IntMap                   as IM
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( getCurrentTime )
import           Internal.Monad
import           Lens.Micro.Platform
import           Reference
import           Text.Megaparsec
import           Text.Megaparsec.Char

prefix :: Text
prefix = "add: "

runAdd :: Args -> CmdInput -> CmdOutput
runAdd args input = do
  let refs     = refsin input
      argsDois = map T.toLower . T.words $ args -- Argument parsing here is trivial.
      -- Error out if no DOIs were given.
  when (null argsDois) $ throwError (prefix <> "no DOIs supplied")
  -- Remove any DOIs that are already in the library, and any that are obviously
  -- not DOIs.
  let alreadyPresentDois = IM.elems refs ^.. each . work . _article . doi
  maybeDois <- forM argsDois $ \argDoi -> if
    | argDoi `elem` alreadyPresentDois -> do
      printError (prefix <> argDoi <> " already in library")
      pure Nothing
    | not (isValidDoi argDoi) -> do
      printError (prefix <> argDoi <> " is not a valid DOI")
      pure Nothing
    | otherwise -> pure (Just argDoi)
  -- Try to get the email to use for Crossref.
  email <- getUserEmail prefix
  -- Fetch the data from Crossref.
  let dois = catMaybes maybeDois
  unless
    (null dois)
    (liftIO $ TIO.putStrLn
      (  "fetching data for DOI"
      <> (if length dois > 1 then "s" else "")
      <> ": "
      <> T.intercalate ", " dois
      <> "..."
      )
    )
  crossrefResponses <- liftIO $ fetchWorks email dois
  let (exceptions, newWorks) = partitionEithers crossrefResponses
  forM_
    exceptions
    (\e -> printError
      (  prefix
      <> "could not get Crossref data for DOI '"
      <> getDoiFromException e
      <> "'. Reason: "
      <> showCrossrefError e
      )
    )
  now <- liftIO getCurrentTime
  let newRefs = map (\w -> Reference w S.empty now now) newWorks
      refsout = IM.fromList $ zip [1 ..] (IM.elems refs ++ newRefs)
  pure $ SCmdOutput refsout Nothing


showCrossrefError :: CrossrefException -> Text
showCrossrefError (CRHttpException _ _) = "Crossref is down or DOI not valid"
showCrossrefError (CRJsonException _ _) = "Crossref provided invalid JSON"
showCrossrefError (CRUnknownWorkException _ w) = "Work type " <> w <> " not currently supported"
showCrossrefError (CROtherException _ _) = "Unknown"
