module Commands.Add (runAdd) where

import Abbotsbury
import Commands.Shared
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.Except
import Data.Either (partitionEithers)
import qualified Data.IntMap as IM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Internal.Path
  ( PDFType (..),
    getPDFPath,
  )
import Reference
import System.Environment
import System.Process

throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ "add: " <> e

runAdd :: Args -> CmdInput -> CmdOutput
runAdd args input = do
  let refs = refsin input
      dois = T.words args -- Argument parsing here is trivial.
      -- Error out if no DOIs were given.
  when (null dois) $ throwErrorWithPrefix "no DOIs supplied"
  -- TODO: minimal verification of DOI legitimacy, to prevent spurious requests.
  -- Try to get the email to use for Crossref.
  maybeEmail <- liftIO getEmailForCrossref
  case maybeEmail of
    Nothing ->
      throwErrorWithPrefix
        ( "no email was specified. "
            <> "Please set either the ABBOT_EMAIL environment variable, "
            <> "or set an email in your .gitconfig file."
        )
    Just email -> do
      -- Fetch the data from Crossref.
      crossrefResponses <- liftIO $ fetchWorks email dois
      let (exceptions, newWorks) = partitionEithers crossrefResponses
      forM_
        exceptions
        ( \e ->
            printError
              ( "add: could not get Crossref data for DOI '"
                  <> getDoiFromException e
                  <> "'"
              )
        )
      now <- liftIO getCurrentTime
      let newRefs = map (\w -> Reference w [] now now) newWorks
          refsout = IM.fromList $ zip [1 ..] (IM.elems refs ++ newRefs)
      pure $ SCmdOutput refsout Nothing

getEmailForCrossref :: IO (Maybe Text)
getEmailForCrossref = do
  maybeEnvvar <- lookupEnv "ABBOT_EMAIL"
  (eitherGit :: Either CE.SomeException String) <-
    CE.try $
      readProcess "git" ["config", "--get", "user.email"] []
  let email = case (maybeEnvvar, eitherGit) of
        (Just e, _) -> Just e
        (Nothing, Right e') -> Just e'
        _ -> Nothing
  pure (T.pack <$> email)
