module Commands.Add
  ( runAdd
  ) where

import           Abbotsbury
import           Commands.Shared
import           Control.Exception
import           Data.Either                    ( partitionEithers )
import qualified Data.IntMap                   as IM
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import           Internal.Monad
import           Internal.Path                  ( PDFType(..)
                                                , getPDFPath
                                                )
import           Reference
import           System.Environment
import           System.Process

prefix :: Text
prefix = "add: "

runAdd :: Args -> CmdInput -> CmdOutput
runAdd args input = do
  let refs = refsin input
      dois = T.words args -- Argument parsing here is trivial.
      -- Error out if no DOIs were given.
  when (null dois) $ throwError (prefix <> "no DOIs supplied")
  -- TODO: minimal verification of DOI legitimacy, to prevent spurious requests.
  -- Try to get the email to use for Crossref.
  email <- getEmailForCrossref
  -- Fetch the data from Crossref.
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
  let newRefs = map (\w -> Reference w [] now now) newWorks
      refsout = IM.fromList $ zip [1 ..] (IM.elems refs ++ newRefs)
  pure $ SCmdOutput refsout Nothing

getEmailForCrossref :: ExceptT Text IO Text
getEmailForCrossref = do
  -- Try to get the environment variable.
  maybeEnvvar <- liftIO $ lookupEnv "ABBOT_EMAIL"
  -- Try to get it from the gitconfig.
  let getGitEmail =
        Just <$> readProcess "git" ["config", "--get", "user.email"] []
  maybeGit <- liftIO $ catch getGitEmail (\(e :: IOException) -> pure Nothing)
  case (maybeEnvvar, maybeGit) of
    (Just e , _      ) -> pure $ T.pack e
    (Nothing, Just e') -> pure $ T.pack e'
    _                  -> throwError
      (  prefix
      <> "no email was specified. "
      <> "Please set either the ABBOT_EMAIL environment variable, "
      <> "or set an email in your .gitconfig file."
      )
