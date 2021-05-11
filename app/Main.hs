module Main where

import Abbotsbury (cite, fetchWorks, getDoiFromException)
import Commands (runCmdWith)
import Commands.Shared
  ( CmdInput (CmdInput),
    ReplCmd (Cd, Nop, Quit),
    SCmdOutput (SCmdOutput),
    runReplParser,
  )
import Control.Monad (forM_, unless, when)
import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
    catchIOError,
  )
import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import Control.Monad.State (StateT (..), evalStateT)
import Data.Either (isLeft)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Internal.MInputT
  ( MInputT,
    defaultSettings,
    mGetInputLine,
    mHandleInterrupt,
    mOutputStrLn,
    mRunInputT,
    mWithInterrupt,
  )
import Lens.Micro.Platform (Lens', lens, use, (.=))
import Options
  ( AbbotCiteOptions (AbbotCiteOptions),
    AbbotCommand (AbbotCite, AbbotMain),
    AbbotMainOptions (AbbotMainOptions),
    abbotParserPrefs,
    parserInfo,
  )
import Options.Applicative (customExecParser)
import Path
  ( expandDirectory,
    readRefs,
    saveRefs,
    setCurrentDirectory,
  )
import Paths_abbotsbury (version)
import Reference (Reference)
import Style (makeError, setBold, setColor, setItalic)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import System.Process (readProcess)

-- | All information needed for the main loop.
data LoopState = LoopState
  { _curDir :: FilePath, -- Current working directory ('wd').
    _oldDir :: FilePath, -- The last wd. Analogous to bash $OLDPWD.
    _dirChanged :: Bool, -- Flag to indicate that the working directory was
    --  changed by the last command, which means that we
    --  should save and reread the references.
    _references :: IntMap Reference -- The references.
  }

-- Template Haskell is cool, but slows down compilation, so we manually generate lenses.
curDir, oldDir :: Lens' LoopState FilePath
curDir = lens _curDir (\ls dir -> ls {_curDir = dir})
oldDir = lens _oldDir (\ls dir -> ls {_oldDir = dir})

dirChanged :: Lens' LoopState Bool
dirChanged = lens _dirChanged (\ls dir -> ls {_dirChanged = dir})

references :: Lens' LoopState (IntMap Reference)
references = lens _references (\ls dir -> ls {_references = dir})

-- | Entry point.
main :: IO ()
main = do
  options <- customExecParser abbotParserPrefs parserInfo
  case options of
    AbbotCite citeOptions -> runAbbotCite citeOptions
    AbbotMain mainOptions -> do
      let AbbotMainOptions startingDirectory version' = mainOptions
      when version' displayVersionAndExit
      startDir <- expandDirectory startingDirectory
      let startState = LoopState startDir startDir True IM.empty
      evalStateT (mRunInputT defaultSettings $ mWithInterrupt loop) startState
      exitSuccess
  where
    displayVersionAndExit :: IO ()
    displayVersionAndExit = putStrLn ("abbot version " <> showVersion version) >> exitSuccess

-- | The main REPL loop of abbot. The `quit` and `cd` functions are implemented
-- here, because they affect the entire state of the program. Other functions
-- are implemented elsewhere.
loop :: MInputT (StateT LoopState IO) ()
loop =
  let exit = pure ()
      save = do
        refs <- use references
        curD <- use curDir
        unless (null refs) (liftIO $ saveRefs refs curD)
   in mHandleInterrupt loop $ do
        curD <- use curDir
        dirC <- use dirChanged

        -- If the directory was changed, read in new data, then turn off the flag
        when dirC $ do
          newRefs <- liftIO . runExceptT $ readRefs curD
          case newRefs of
            Right nrefs -> do
              references .= nrefs
              unless
                (null nrefs)
                ( mOutputStrLn
                    ("Read in " ++ show (length nrefs) ++ " references.")
                )
            Left errmsg -> printErr errmsg
        dirChanged .= False

        -- Show the prompt and get the command
        cwd <- liftIO $ expandDirectory curD
        input <- prompt cwd

        -- Parse and run the command
        case fmap T.pack input of
          Nothing -> save >> exit -- Ctrl-D
          Just cmdArgs -> case runReplParser cmdArgs of
            Left _ ->
              printErr ("command '" <> cmdArgs <> "' not recognised") >> loop
            -- Special commands that we need to handle in main loop
            Right Nop -> loop
            Right Quit -> mOutputStrLn "quitting..." >> save >> exit
            Right (Cd fp) -> do
              -- Check for 'cd -'
              newD <-
                if fp == "-"
                  then use oldDir
                  else liftIO $ expandDirectory $ T.unpack fp
              -- If the new directory is different, then change the working directory
              when (newD /= curD) $
                catchIOError
                  ( do
                      save
                      liftIO (setCurrentDirectory newD)
                      curDir .= newD
                      oldDir .= curD
                      dirChanged .= True
                  )
                  (\_ -> printErr (T.pack newD <> ": no such directory"))
              loop
            -- All other commands
            Right otherCmd -> do
              currentDir <- use curDir
              currentRefs <- use references
              let cmdInput = CmdInput currentDir currentRefs Nothing
              -- TODO: In principle, all IO exceptions should be caught here, as
              -- they are not exhaustively encoded in the ExceptT error type.
              cmdOutput <- liftIO . runExceptT $ runCmdWith otherCmd cmdInput
              case cmdOutput of
                Left err -> printErr err >> loop
                Right (SCmdOutput newRefs _) -> do
                  references .= newRefs
                  save
                  loop

-- | Generates the prompt for the main loop.
prompt :: FilePath -> MInputT (StateT LoopState IO) (Maybe String)
prompt fp =
  mGetInputLine . T.unpack $
    mconcat
      [ setColor "plum" $ "(" <> T.pack fp <> ") ",
        setColor "hotpink" . setBold . setItalic $ "peep > "
      ]

-- | Prints an error in the main loop.
printErr :: Text -> MInputT (StateT LoopState IO) ()
printErr = mOutputStrLn . T.unpack . makeError

runAbbotCite :: AbbotCiteOptions -> IO ()
runAbbotCite citeOptions = do
  let AbbotCiteOptions dois style' format' useGit = citeOptions
  email <-
    if useGit
      then
        ( do
            e <- T.strip . T.pack <$> readProcess "git" ["config", "--get", "user.email"] ""
            when (T.null e) (exitWithError noGitEmailText)
            pure e
        )
      else
        ( do
            maybeE <- fmap (T.strip . T.pack) <$> lookupEnv "ABBOT_EMAIL"
            when (isNothing maybeE) (exitWithError noAbbotEmailText)
            pure (fromJust maybeE)
        )
  eitherWorks <- fetchWorks email dois
  forM_ eitherWorks $ \case
    Left exc -> do
      displayError
        ( "could not find metadata for DOI '"
            <> getDoiFromException exc
            <> "' on Crossref"
        )
    Right work -> do
      TIO.putStrLn $ cite style' format' work
  if not (any isLeft eitherWorks) then exitSuccess else exitFailure
  where
    noGitEmailText :: Text
    noGitEmailText =
      "The --use-git-email switch was used, but `abbot cite` could not get your email via `git config`.\n"
        <> "       Abbot needs this information to make 'polite' calls to the Crossref API.\n"
        <> "       See https://github.com/CrossRef/rest-api-doc#etiquette for more information."
    noAbbotEmailText :: Text
    noAbbotEmailText =
      "Please set the ABBOT_EMAIL environment variable to your email before using `abbot cite`.\n"
        <> "       Abbot needs this information to make 'polite' calls to the Crossref API.\n"
        <> "       See https://github.com/CrossRef/rest-api-doc#etiquette for more information."
    displayError :: Text -> IO ()
    displayError t = TIO.hPutStrLn stderr t'
      where
        t' = (setColor "tomato" . setBold $ "error: ") <> setColor "tomato" t
    exitWithError :: Text -> IO ()
    exitWithError t = displayError t >> exitFailure
