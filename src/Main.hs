{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Exception
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.State
import           Data.Char                      ( isSpace )
import qualified Data.Colour.Names             as CNames
import           Options.Applicative     hiding ( Parser )
import qualified Options.Applicative           as O
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath

{- | All information needed for the main loop.
-}
data LoopState = LoopState { curDir :: FilePath
                           , oldDir :: FilePath
                           , references :: [Reference]
                           }
type Reference = String -- For now.

instance MonadState s m => MonadState s (InputT m) where
  get = lift get
  put = lift . put

main :: IO ()
main = do
  options  <- execParser opts
  startDir <- expandDirectory (startingDirectory options)
  let startState = LoopState startDir startDir []
  evalStateT (runInputT defaultSettings loop) startState
 where
  loop :: InputT (StateT LoopState IO) ()
  loop = do
    curDirectory <- gets curDir
    cwd          <- liftIO $ expandDirectory curDirectory
    minput       <- getInputLine $ makePrompt cwd
    case minput of
      Nothing                 -> pure ()
      Just ('c' : 'd' : rest) -> do
        let rest'      = dropWhile isSpace rest
            gotoOldDir = filter (not . isSpace) rest == "-"   -- `cd -`
        newDirectory <- if gotoOldDir
                           then gets oldDir
                           else liftIO $ expandDirectory rest'
        Catch.catchIOError
          (  liftIO (setCurrentDirectory newDirectory)
          >> put (LoopState newDirectory curDirectory [])
          >> loop
          )
          (  const
          $  (liftIO . putStrLn $ newDirectory ++ ": no such directory")
          >> loop
          )
      Just "quit" -> pure ()
      Just input  -> do
        outputStrLn $ "Input was: " ++ input
        loop


{- | Generates the prompt, including the ANSI escape characters for colours
- and styling.
-}
makePrompt :: FilePath -> String
makePrompt fp = mconcat [ setSGRCode [SetRGBColor Foreground CNames.plum]
                        , "("
                        , fp
                        , ")"
                        , setSGRCode [ SetRGBColor Foreground CNames.salmon
                                     , SetConsoleIntensity BoldIntensity
                                     , SetItalicized True
                                     ]
                        , " peep > "
                        , setSGRCode [Reset]
                        ]


-- Command-line option parsing for the executable itself.
newtype Options = Options
             { startingDirectory :: FilePath
             }
             deriving (Show)
opts :: ParserInfo Options
opts = info (helper <*> optionParser) ( fullDesc
                                      <> progDesc "Minimalistic command-line reference manager"
                                      )
optionParser :: O.Parser Options
optionParser = Options <$> strOption
                             (  short 'd'
                             <> long "directory"
                             <> help "Directory to start in"
                             <> value "."
                             <> showDefaultWith (const "current working directory")
                             )

{- | Expands relative paths and tildes in directories. Tilde expansion does not
work with arbitrary users (`~user/path/to/file`), only the currently logged in user
(`~/path/to/file`).
Relative paths are resolved with respect to the current working directory.
-}
expandDirectory :: FilePath -> IO FilePath
expandDirectory fp =
  let components = splitPath . dropTrailingPathSeparator $ fp
  in  (case components of
        []            -> getHomeDirectory
        ["/"        ] -> pure fp
        ["~"        ] -> getHomeDirectory
        ("~/" : rest) -> do
          home <- getHomeDirectory
          pure $ joinPath ((home ++ "/") : rest)
        _ -> pure fp
      )
        >>= canonicalizePath
