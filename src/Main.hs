{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Abbot.Commands
import           Abbot.Monad
import           Abbot.Path
import           Abbot.Style

import           Control.Exception
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Options.Applicative     hiding ( Parser )
import qualified Options.Applicative           as O
import           System.Console.Haskeline

-- | Entry point.
main :: IO ()
main = do
  options  <- execParser replOptionInfo
  startDir <- expandDirectory (startingDirectory options)
  let startState = LoopState startDir startDir []
  evalStateT (runInputT defaultSettings $ withInterrupt loop) startState

-- | All information needed for the main loop.
data LoopState = LoopState
  { curDir     :: FilePath
  , oldDir     :: FilePath
  , references :: [Reference]
  }

-- | The main REPL loop of abbot. The `quit` and `cd` functions are implemented
-- here, because they affect the entire state of the program. Other functions
-- are implemented elsewhere.
loop :: InputT (StateT LoopState IO) ()
loop =
  let exit = return ()
  in
    handleInterrupt loop $ do
      curDirectory <- gets curDir
      refs         <- gets references
      cwd          <- liftIO $ expandDirectory curDirectory
      minput       <- prompt cwd
      case fmap T.pack minput of
        Nothing  -> exit                  -- Ctrl-D
        Just cmd -> case runReplParser cmd of
          Left  _          -> printErr ("unrecognised command " <> cmd) >> loop
          Right (Nop , _ ) -> loop
          Right (Quit, _ ) -> exit
          Right (Cd  , fp) -> do
            newDirectory <- if fp == "-"
              then gets oldDir
              else liftIO . expandDirectory . T.unpack $ fp
            catchIOError
              (  liftIO (setCurrentDirectory newDirectory)
              >> put (LoopState newDirectory curDirectory [])
              >> loop
              )
              (\e ->
                printErr (T.pack newDirectory <> ": no such directory") >> loop
              )
          Right (cmd, args) -> case runCommand cmd args refs of
            Left  err          -> printErr err >> loop
            Right (_, actions) -> liftIO (sequence_ actions) >> loop


-- | Generates the prompt.
prompt :: FilePath -> InputT (StateT LoopState IO) (Maybe String)
prompt fp = getInputLine . T.unpack $ mconcat
  [ setColor "plum" . T.pack $ "(" ++ fp ++ ") "
  , setColor "hotpink" . setBold . setItalic $ "peep > "
  ]

-- | Prints an error.
printErr :: Text -> InputT (StateT LoopState IO) ()
printErr errMsg = outputStrLn . T.unpack $ mconcat
  [ setColor "red" "error: "
  , setColor "coral" errMsg
  ]


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
