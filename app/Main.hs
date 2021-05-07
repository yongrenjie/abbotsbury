{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Abbotsbury
import           Commands
import           Commands.Shared
import           Options
import           Path
import           Reference
import           Style


import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                , catchIOError
                                                )
import           Control.Monad.Except
import           Control.Monad.State
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           Options.Applicative            ( execParser )
import           System.Console.Haskeline      as HL
import           System.Environment             ( lookupEnv )
import           System.Exit
import           System.IO                      ( stderr )


-- | All information needed for the main loop.
data LoopState = LoopState
  { _curDir     :: FilePath         -- Current working directory ('wd').
  , _oldDir     :: FilePath         -- The last wd. Analogous to bash $OLDPWD.
  , _dirChanged :: Bool             -- Flag to indicate that the working directory was
                                    --  changed by the last command, which means that we
                                    --  should save and reread the references.
  , _references :: IntMap Reference -- The references.
  }
-- Template Haskell is cool, but slows down compilation, so we manually generate lenses.
curDir, oldDir :: Lens' LoopState FilePath
curDir = lens _curDir (\ls dir -> ls { _curDir = dir })
oldDir = lens _oldDir (\ls dir -> ls { _oldDir = dir })
dirChanged :: Lens' LoopState Bool
dirChanged = lens _dirChanged (\ls dir -> ls { _dirChanged = dir })
references :: Lens' LoopState (IntMap Reference)
references = lens _references (\ls dir -> ls { _references = dir })


-- InputT doesn't provide instances of MTL classes, so we need to do it ourselves
-- The approach is taken from the (no longer working) haskeline-class package, which
-- can be found at https://hackage.haskell.org/package/haskeline-class
newtype MInputT m a = MInputT { unMInputT :: HL.InputT m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch)
instance MonadState s m => MonadState s (MInputT m) where
  get   = lift get
  put   = lift . put
  state = lift . state
mRunInputT :: (MonadIO m, MonadMask m) => HL.Settings m -> MInputT m a -> m a
mRunInputT s m = HL.runInputT s (unMInputT m)
mWithInterrupt :: (MonadIO m, MonadMask m) => MInputT m a -> MInputT m a
mWithInterrupt = MInputT . HL.withInterrupt . unMInputT
mHandleInterrupt
  :: (MonadIO m, MonadMask m) => MInputT m a -> MInputT m a -> MInputT m a
mHandleInterrupt catchA tryA =
  MInputT $ HL.handleInterrupt (unMInputT catchA) (unMInputT tryA)
mGetInputLine :: (MonadIO m, MonadMask m) => String -> MInputT m (Maybe String)
mGetInputLine = MInputT . HL.getInputLine
mGetInputChar :: (MonadIO m, MonadMask m) => String -> MInputT m (Maybe Char)
mGetInputChar = MInputT . HL.getInputChar
mOutputStr :: MonadIO m => String -> MInputT m ()
mOutputStr = MInputT . HL.outputStr
mOutputStrLn :: MonadIO m => String -> MInputT m ()
mOutputStrLn = MInputT . HL.outputStrLn


-- | Entry point.
main :: IO ()
main = do
  options <- execParser parserInfo
  case options of
    AbbotMain mainOptions -> do
      startDir <- expandDirectory (startingDirectory mainOptions)
      let startState = LoopState startDir startDir True IM.empty
      evalStateT (mRunInputT defaultSettings $ mWithInterrupt loop) startState
      exitSuccess
    AbbotCite citeOptions -> do
      maybeEmail <- lookupEnv "ABBOT_EMAIL"
      case maybeEmail of
        Nothing -> do
          exitWithError noAbbotEmailText
        Just email -> do
          let AbbotCiteOptions doi' style' format' = citeOptions
          eitherWork <- fetchWork (T.pack email) doi'
          case eitherWork of
            Left exception -> do
              exitWithError
                ("failed to get Crossref metadata for DOI '" <> doi' <> "'")
            Right work -> do
              TIO.putStrLn $ cite style' format' work
              exitSuccess
 where
  exitWithError :: Text -> IO ()
  exitWithError t = TIO.hPutStrLn stderr t' >> exitFailure
    where t' = (setColor "tomato" . setBold $ "error: ") <> setColor "tomato" t
  noAbbotEmailText :: Text
  noAbbotEmailText =
    "Please set the ABBOT_EMAIL environment variable to your email before using `abbot cite`.\n"
      <> "       Abbot needs this information to make 'polite' calls to the Crossref API.\n"
      <> "       See https://github.com/CrossRef/rest-api-doc#etiquette for more information."


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

  in  mHandleInterrupt loop $ do
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
                (mOutputStrLn
                  ("Read in " ++ show (length nrefs) ++ " references.")
                )
            Left errmsg -> printErr errmsg
        dirChanged .= False

        -- Show the prompt and get the command
        cwd   <- liftIO $ expandDirectory curD
        input <- prompt cwd

        -- Parse and run the command
        case fmap T.pack input of
          Nothing      -> save >> exit   -- Ctrl-D
          Just cmdArgs -> case runReplParser cmdArgs of
            Left _ ->
              printErr ("command '" <> cmdArgs <> "' not recognised") >> loop
            -- Special commands that we need to handle in main loop
            Right Nop -> loop
            Right Quit -> mOutputStrLn "quitting..." >> save >> exit
            Right (Cd fp) -> do
              -- Check for 'cd -'
              newD <- if fp == "-"
                then use oldDir
                else liftIO $ expandDirectory $ T.unpack fp
              -- If the new directory is different, then change the working directory
              when (newD /= curD) $ catchIOError
                (do
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
              currentDir  <- use curDir
              currentRefs <- use references
              let cmdInput = CmdInput Nothing currentDir currentRefs
              -- TODO: In principle, all IO exceptions should be caught here, as
              -- they are not exhaustively encoded in the ExceptT error type.
              cmdOutput   <- liftIO . runExceptT $ runCmdWith otherCmd cmdInput
              case cmdOutput of
                Left  err          -> printErr err >> loop
                Right (SCmdOutput newRefs _) -> do
                  references .= newRefs
                  save
                  loop


-- | Generates the prompt for the main loop.
prompt :: FilePath -> MInputT (StateT LoopState IO) (Maybe String)
prompt fp = mGetInputLine . T.unpack $ mconcat
  [ setColor "plum" $ "(" <> T.pack fp <> ") "
  , setColor "hotpink" . setBold . setItalic $ "peep > "
  ]


-- | Prints an error in the main loop.
printErr :: Text -> MInputT (StateT LoopState IO) ()
printErr = mOutputStrLn . T.unpack . makeError


-- | Copies to clipboard. Not done yet (obviously.)
copyToClipboard :: Text -> IO ()
copyToClipboard = TIO.putStrLn
