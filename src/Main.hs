{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Abbot.Commands
import           Abbot.Path
import           Abbot.Style

import           Abbotsbury

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                , catchIOError
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           Options.Applicative     hiding ( Parser )
import qualified Options.Applicative           as O
import           System.Console.Haskeline      as HL


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


-- | All information needed for the main loop.
data LoopState = LoopState
  { _curDir     :: FilePath    -- Current working directory ('wd').
  , _oldDir     :: FilePath    -- The last wd. Analogous to bash $OLDPWD.
  , _dirChanged :: Bool        -- Flag to indicate that the wd was changed by the last command,
                               --   which means that we should save and reread the references.
  , _references :: [Reference] -- The reference list.
  }
makeLenses ''LoopState


-- | Entry point.
main :: IO ()
main = do
  options  <- execParser replOptionInfo
  startDir <- expandDirectory (startingDirectory options)
  let startState = LoopState startDir startDir True []
  evalStateT (mRunInputT defaultSettings $ mWithInterrupt loop) startState


-- | The main REPL loop of abbot. The `quit` and `cd` functions are implemented
-- here, because they affect the entire state of the program. Other functions
-- are implemented elsewhere.
loop :: MInputT (StateT LoopState IO) ()
loop =
  let exit = pure ()
  in
    mHandleInterrupt loop $ do
      LoopState curD oldD dirC refs <- get
      -- Read in new data if the directory was changed, then turn off the flag
      when dirC $ do
        newRefs <- liftIO (readRefs curD)
        case newRefs of
             Right nrefs -> references .= nrefs >> mOutputStrLn "Yay!"
             Left errmsg -> printErr errmsg
      dirChanged .= False
      -- Show the prompt and get the command
      cwd   <- liftIO $ expandDirectory curD
      input <- prompt cwd
      -- Parse and run the command
      case fmap T.pack input of
        Nothing      -> exit                  -- Ctrl-D
        Just cmdArgs -> case runReplParser cmdArgs of
          Left _ ->
            printErr ("command '" <> cmdArgs <> "' not recognised") >> loop
          Right (Nop , _ ) -> loop
          Right (Quit, _ ) -> mOutputStrLn "quitting..." >> exit
          Right (Cd  , fp) -> do
            -- Check for 'cd -'
            newD <- if fp == "-"
              then pure oldD
              else liftIO $ expandDirectory $ T.unpack fp
            -- If the new directory is different, then change the working directory
            when (newD /= curD) $ catchIOError
              (  liftIO (setCurrentDirectory newD)
              >> put (LoopState newD curD True [])
              )
              (\_ -> printErr (T.pack newD <> ": no such directory"))
            loop
          Right (cmd, args) -> do
            cmdOutput <- liftIO $ runCommand cmd args refs
            case cmdOutput of
              Left  err          -> printErr err >> loop
              Right (newRefs, _) -> do
                put (LoopState curD oldD False newRefs)
                loop

-- | Generates the prompt.
prompt :: FilePath -> MInputT (StateT LoopState IO) (Maybe String)
prompt fp = mGetInputLine . T.unpack $ mconcat
  [ setColor "plum" $ "(" <> T.pack fp <> ") "
  , setColor "hotpink" . setBold . setItalic $ "peep > "
  ]

-- | Prints an error.
printErr :: Text -> MInputT (StateT LoopState IO) ()
printErr errMsg = mOutputStrLn . T.unpack $ mconcat
  [setColor "red" "error: ", setColor "coral" errMsg]

-- | Copies to clipboard. Not done yet (obviously.)
copyToClipboard :: Text -> IO ()
copyToClipboard = TIO.putStrLn

-- Command-line option parsing for the executable itself.
newtype ReplOptions = ReplOptions
             { startingDirectory :: FilePath
             }
             deriving (Show)

replOptionInfo :: ParserInfo ReplOptions
replOptionInfo = info
  (helper <*> replOptionParser)
  (fullDesc <> progDesc "Minimalistic command-line reference manager")

replOptionParser :: O.Parser ReplOptions
replOptionParser = ReplOptions <$> strOption
  (  short 'd'
  <> long "directory"
  <> help "Directory to start in"
  <> value "."
  <> showDefaultWith (const "current working directory")
  )
