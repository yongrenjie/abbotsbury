{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import           Abbot.Commands
import           Abbot.Path
import           Abbot.Style

import           Abbotsbury

import           Control.Monad.Catch            ( catchIOError )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Options.Applicative     hiding ( Parser )
import qualified Options.Applicative           as O
import           System.Console.Haskeline

-- | Entry point.
main :: IO ()
main = do
  options  <- execParser replOptionInfo
  startDir <- expandDirectory (startingDirectory options)
  let startState = LoopState startDir startDir True []
  evalStateT (runInputT defaultSettings $ withInterrupt loop) startState


-- | All information needed for the main loop.
data LoopState = LoopState
  { curDir     :: FilePath     -- Current working directory ('wd').
  , oldDir     :: FilePath     -- The last wd. Analogous to bash $OLDPWD.
  , dirChanged :: Bool         -- Flag to indicate that the wd was changed by the last command,
                               --   which means that we should save and reread the references.
  , references :: [Reference]  -- The reference list.
  }

-- | The main REPL loop of abbot. The `quit` and `cd` functions are implemented
-- here, because they affect the entire state of the program. Other functions
-- are implemented elsewhere.
-- OK, instead of using lift . put everywhere, ideally I would make a MonadState
-- instance for this. But I'm afraid that as of now, I don't know how to do that.
loop :: InputT (StateT LoopState IO) ()
loop =
  let exit = return ()
  in
    handleInterrupt loop $ do
      ls@(LoopState curD oldD dirC refs) <- lift get
      -- Read in new data if the directory was changed, then turn off the flag
      when
        dirC
        (liftIO $ putStrLn
          "directory changed! in general we should read/write here..."
        )
      lift $ put (ls { dirChanged = False })
      -- Show the prompt and get the command
      cwd   <- liftIO $ expandDirectory curD
      input <- prompt cwd
      -- Parse and run the command
      case fmap T.pack input of
        Nothing  -> exit                  -- Ctrl-D
        Just cmdArgs -> case runReplParser cmdArgs of
          Left _ -> printErr ("command '" <> cmdArgs <> "' not recognised") >> loop
          Right (Nop , _ ) -> loop
          Right (Quit, _ ) -> outputStrLn "quitting..." >> exit
          Right (Cd  , fp) -> do
            -- Check for 'cd -'
            newD <- if fp == "-"
              then lift $ gets oldDir
              else liftIO $ expandDirectory . T.unpack $ fp
            -- If the new directory is different, then change the working directory
            when (newD /= curD) $ catchIOError
                (  liftIO (setCurrentDirectory newD)
                >> lift (put (LoopState newD curD True []))
                )
                (\_ -> printErr (T.pack newD <> ": no such directory"))
            loop
          Right (cmd, args) -> do
            cmdOutput <- liftIO $ runCommand cmd args refs
            case cmdOutput of
              Left  err          -> printErr err >> loop
              Right (newRefs, _) -> do
                lift . put $ LoopState curD oldD False newRefs
                loop

-- | Generates the prompt.
prompt :: FilePath -> InputT (StateT LoopState IO) (Maybe String)
prompt fp = getInputLine . T.unpack $ mconcat
  [ setColor "plum" $ "(" <> T.pack fp <> ") "
  , setColor "hotpink" . setBold . setItalic $ "peep > "
  ]

-- | Prints an error.
printErr :: Text -> InputT (StateT LoopState IO) ()
printErr errMsg = outputStrLn . T.unpack $ mconcat
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
