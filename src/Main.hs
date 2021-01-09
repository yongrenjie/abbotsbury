module Main where

import           Abbot.Monad
import           Abbot.Parse
import           Abbot.Path

import           Control.Exception
import           Data.Char                      ( isSpace )
import qualified Data.Colour.Names             as CNames
import           Options.Applicative     hiding ( Parser )
import qualified Options.Applicative           as O
import           System.Console.ANSI
import           System.Console.Haskeline

{- | All information needed for the main loop.
-}
data LoopState = LoopState
  { curDir     :: FilePath
  , oldDir     :: FilePath
  , references :: [Reference]
  }
type Reference = String -- For now.


main :: IO ()
main = do
  options  <- execParser replOptionInfo
  startDir <- expandDirectory (startingDirectory options)
  let startState = LoopState startDir startDir []
  evalStateT (runInputT defaultSettings $ withInterrupt loop) startState

-- | The main REPL loop of abbot.
loop :: InputT (StateT LoopState IO) ()
loop =
  let exit = return ()
  in
    handleInterrupt loop $ do
      curDirectory <- gets curDir
      cwd          <- liftIO $ expandDirectory curDirectory
      minput       <- getInputLine $ makePrompt cwd
      case minput of
        Nothing  -> exit                  -- Ctrl-D
        Just cmd -> case runReplParser cmd of
          Left  _       -> outputStrLn (makeErrStr $ "unrecognised command " ++ cmd) >> loop
          Right Quit    -> exit
          Right (Cd fp) -> do
            let gotoOldDir = filter (not . isSpace) fp == "-"   -- `cd -`
            newDirectory <- if gotoOldDir
              then gets oldDir
              else liftIO $ expandDirectory fp
            catchIOError
              (  liftIO (setCurrentDirectory newDirectory)
              >> put (LoopState newDirectory curDirectory [])
              >> loop
              )
              (\e -> outputStrLn (newDirectory ++ ": no such directory") >> loop
              )


{- | Generates the prompt, including the ANSI escape characters for colours
- and styling.
-}
makePrompt :: FilePath -> String
makePrompt fp = mconcat
  [ setSGRCode [SetRGBColor Foreground CNames.plum]
  , "("
  , fp
  , ")"
  , setSGRCode
    [ SetRGBColor Foreground CNames.salmon
    , SetConsoleIntensity BoldIntensity
    , SetItalicized True
    ]
  , " peep > "
  , setSGRCode [Reset]
  ]

makeErrStr :: String -> String
makeErrStr err = mconcat
  [ setSGRCode [SetRGBColor Foreground CNames.tomato]
  , "error: "
  , setSGRCode [Reset]
  , err
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
