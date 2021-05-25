{-# LANGUAGE MultiWayIf #-}

module Commands.Add
  ( runAdd
  ) where

import           Abbotsbury
import           Commands.Shared
import           Data.Either                    ( partitionEithers )
import qualified Data.IntMap                   as IM
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( getCurrentTime )
import           Internal.Monad
import           Internal.Path                  ( PDFType(..)
                                                , getPDFPath
                                                )
import           Lens.Micro.Platform
import           Reference
import           Text.Regex.TDFA

prefix :: Text
prefix = "add: "

runAdd :: Args -> CmdInput -> CmdOutput
runAdd args input = do
  let refs     = refsin input
      argsDois = T.words args -- Argument parsing here is trivial.
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
      (  "Fetching data for DOI"
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
      <> "'"
      )
    )
  now <- liftIO getCurrentTime
  let newRefs = map (\w -> Reference w S.empty now now) newWorks
      refsout = IM.fromList $ zip [1 ..] (IM.elems refs ++ newRefs)
  pure $ SCmdOutput refsout Nothing

-- | This regex check is VERY basic, I don't intend for it to be very selective.
-- It's just to weed out extremely obvious mistakes.
isValidDoi :: DOI -> Bool
isValidDoi doi = doi =~ ("\\`10\\.[0-9]{4,9}/[^[:space:]]+\\'" :: Text)
