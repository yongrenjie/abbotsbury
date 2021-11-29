{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Abbotsbury                     ( cite
                                                , fetchWorks
                                                , getDoiFromException
                                                , htmlFormat
                                                )
import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.Table
import           Commands                       ( runCmdWith )
import           Commands.Shared                ( AbbotCmd(Cd, Nop, Quit)
                                                , CmdInput(CmdInput)
                                                , SCmdOutput(SCmdOutput)
                                                , runReplParser
                                                )
import           Control.Exception              ( IOException
                                                , catch
                                                )
import           Control.Monad.Catch            ( catchIOError )
import           Control.Monad.State            ( StateT(..)
                                                , evalStateT
                                                )
import           Data.Either                    ( isLeft
                                                , partitionEithers
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Version                   ( showVersion )
import           Graphics.Vty
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Output.Interface
import           Internal.Copy
import           Internal.MInputT               ( MInputT
                                                , defaultSettings
                                                , mGetInputLine
                                                , mHandleInterrupt
                                                , mOutputStrLn
                                                , mRunInputT
                                                , mWithInterrupt
                                                )
import           Internal.Monad
import           Internal.Path                  ( expandDirectory
                                                , readRefs
                                                , saveRefs
                                                , setCurrentDirectory
                                                )
import           Internal.PrettyRef
import           Internal.Style                 ( makeError
                                                , setBold
                                                , setColor
                                                , setItalic
                                                )
import           Internal.Types
import           Lens.Micro.Platform
import           Options                        ( AbbotCiteOptions
                                                  ( AbbotCiteOptions
                                                  )
                                                , AbbotCommand
                                                  ( AbbotCite
                                                  , AbbotMain
                                                  )
                                                , AbbotMainOptions
                                                  ( AbbotMainOptions
                                                  )
                                                , AbbotVerbosity(..)
                                                , CopyOption(..)
                                                , abbotParserPrefs
                                                , parserInfo
                                                )
import           Options.Applicative            ( customExecParser )
import           Paths_abbotsbury               ( version )
import           Reference                      ( Reference )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( ExitCode(..)
                                                , exitFailure
                                                , exitSuccess
                                                , exitWith
                                                )
import           System.IO                      ( stderr )
import           System.Process                 ( readProcess
                                                , waitForProcess
                                                )

-- | All information needed for the main loop.
data AbbotState = AbbotState
  { _curDir       :: FilePath         -- Current working directory ('wd').
  , _oldDir       :: FilePath         -- The last wd. Analogous to bash $OLDPWD.
  , _dirChanged   :: Bool             -- Did last command change wd?
  , _verbosity    :: AbbotVerbosity   -- Verbosity level
  , _activeWidget :: WidgetID         -- Currently active widget
  , _refs         :: IntMap Reference -- The references
  , _refsWidget   :: [Widget RName]   -- Pretty-printed references
  , _refsCurrent  :: Int              -- Currently active reference
  , _commandText  :: Text             -- Current command
  , _searchText   :: Text             -- Current search query
  }

makeLenses ''AbbotState

-- Create a new AbbotState by reading in references.
mkAbbotState :: FilePath -> FilePath -> Bool -> IO AbbotState
mkAbbotState curDir oldDir dirChanged = do
  eRefs <- runExceptT $ readRefs curDir
  let refs = case eRefs of
              Left _ -> IM.empty
              Right r -> r
      refsCurrent = 1
  refsWidget <- prettify curDir (Just 5) refsCurrent (IM.assocs refs)
  pure $ AbbotState { _curDir = curDir
                    , _oldDir = oldDir
                    , _dirChanged = dirChanged
                    , _verbosity = Quiet
                    , _activeWidget = References
                    , _refs = refs
                    , _refsWidget = refsWidget
                    , _refsCurrent = refsCurrent
                    , _commandText = "Command bar here..."
                    , _searchText  = "Search bar here..." }

app :: App AbbotState e RName
app = App
  { appDraw         = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap
                        mempty
                        [ ("bold"       , currentAttr `withStyle` bold)
                        , ("selectedRef", currentAttr `withStyle` bold)
                        -- scrollbarAttr must be set so that it doesn't get
                        -- turned blue
                        , (scrollbarAttr, defAttr)
                        , ("activeWidget", currentAttr `withBackColor` rgbColor 255 255 215)
                        ]
  }

-- Recalculates the refWidget component of the state. 
updateState :: AbbotState -> IO AbbotState
updateState ast = do
  let cwd   = ast ^. curDir
      rs  = ast ^. refs
      n = ast ^. refsCurrent
  refsWidget <- prettify cwd (Just 5) n (IM.assocs rs)
  pure $ ast { _refsWidget = refsWidget }

-- Render the TUI.
draw :: AbbotState -> [Widget RName]
draw ast =
  let
    highlightIf =
      \w -> if w == ast ^. activeWidget then withBorderStyle unicodeBold else id
    refsW = highlightIf References $ head $ ast ^. refsWidget
    cmdW =
      highlightIf Command
        $ border
        . hLimit 40
        . padRight Max
        $ if T.null (ast ^. commandText)
            then txtWrap (T.replicate 40 " ")
            else txtWrap (ast ^. commandText)
    searchW =
      highlightIf Search
        $ border
        . hLimit 30
        . padRight Max
        $ if T.null (ast ^. searchText)
            then txtWrap (T.replicate 30 " ")
            else txtWrap (ast ^. searchText)
    bottomW = vLimit 2 . padRight Max $ hBox [cmdW, searchW]
  in
    [vBox [refsW, bottomW]]

-- Handle events.
handleEvent
  :: AbbotState -> BrickEvent RName e -> EventM RName (Next AbbotState)
handleEvent ast e = do
  let continueWithUpdate :: AbbotState -> EventM RName (Next AbbotState)
      continueWithUpdate astate = do
        updatedState <- liftIO $ updateState astate
        continue updatedState
  case e of
    VtyEvent vtye -> case vtye of
      -- q: quit
      EvKey (KChar 'q') []      -> halt ast
      -- <C-L>: redraw
      EvKey (KChar 'l') [MCtrl] -> continueWithUpdate ast
      -- Arrow-down: move down
      EvKey KDown       []      -> do
        let rs   = ast ^. refs
            curr = ast ^. refsCurrent
        continueWithUpdate $ case IM.lookupGT curr rs of
          Just (next, _) -> ast { _refsCurrent = next }
          Nothing        -> ast
      -- Arrow-down: move down
      EvKey KUp [] -> do
        let rs   = ast ^. refs
            curr = ast ^. refsCurrent
        continueWithUpdate $ case IM.lookupLT curr rs of
          Just (prev, _) -> ast { _refsCurrent = prev }
          Nothing        -> ast
      -- Colon: highlight the command window
      EvKey (KChar ':') [] -> continue $ ast { _activeWidget = Command }
      -- Forward slash: highlight the search window
      EvKey (KChar '/') [] -> continue $ ast { _activeWidget = Search }
      -- Escape: go back to references widget and clear the current widget
      EvKey KEsc        [] -> do
        let
          currActive = ast ^. activeWidget
          newCommandText =
            if currActive == Command then "" else ast ^. commandText
          newSearchText =
            if currActive == Search then "" else ast ^. searchText
        continueWithUpdate $ ast { _activeWidget = References
                                 , _commandText  = newCommandText
                                 , _searchText   = newSearchText
                                 }
      -- Something else: show it in the command window (for now)
      other -> continueWithUpdate
        $ ast { _commandText = "You entered: " <> T.pack (show other) }
    _ -> continue ast

-- | Entry point.
main :: IO ()
main = do
  options <- customExecParser abbotParserPrefs parserInfo
  case options of
    AbbotCite citeOptions -> runAbbotCite citeOptions
    AbbotMain mainOptions -> do
      let AbbotMainOptions startingDirectory verbosity version' = mainOptions
      when version' displayVersionAndExit
      -- Initialise state
      startDir     <- expandDirectory startingDirectory
      initialState <- mkAbbotState startDir startDir False
      initialVty   <- mkVty $ defaultConfig { termName = Just "xterm-256color" }
      endState     <- customMain initialVty
                                 (pure initialVty)
                                 Nothing
                                 app
                                 initialState
      exitSuccess
 where
  displayVersionAndExit :: IO ()
  displayVersionAndExit =
    putStrLn ("abbot version " <> showVersion version) >> exitSuccess





-- `abbot cite` subcommand --
-----------------------------

-- | Run the `abbot cite` command.
-- Exit codes are:
--     0 if at least one DOI was found
--     1 if something completely wrong happened (i.e. didn't even get to the
--        point where DOIs were being looked up)
--     2 if all DOIs failed to resolve
runAbbotCite :: AbbotCiteOptions -> IO ()
runAbbotCite citeOptions = do
  let AbbotCiteOptions dois (style', format', copy') useGit = citeOptions
  email <- if useGit
    then
      (do
        e <- T.strip . T.pack <$> readProcess
          "git"
          ["config", "--get", "user.email"]
          ""
        when (T.null e) (exitWithError noGitEmailText)
        pure e
      )
    else
      (do
        maybeE <- fmap (T.strip . T.pack) <$> lookupEnv "ABBOT_EMAIL"
        when (isNothing maybeE) (exitWithError noAbbotEmailText)
        pure (fromJust maybeE)
      )
  eitherWorks <- fetchWorks email dois
  let (excs, works) = partitionEithers eitherWorks
  -- Print the errors
  forM_ excs $ \exc -> displayError (  "could not find metadata for DOI '"
                                     <> getDoiFromException exc
                                     <> "' on Crossref"
                                     )
  if null works then exitWith (ExitFailure 2)
                else do
    -- Print the citations
    let citations = T.intercalate "\n" $ map (cite style' format') works
    TIO.putStrLn citations
    -- Copy them if requested
    maybeHandle <- case copy' of
         NoCopy -> pure Nothing
         CopyAsText -> copy citations
         CopyAsRtf -> do
           let htmlCitations = map (cite style' htmlFormat) works
           case htmlCitations of
                [c] -> copyHtmlAsRtf c
                _   -> copyHtmlLinesAsRtf htmlCitations
    -- We must block until the copying finishes, or else the thread running the
    -- copy will be silently killed when the main process exits!
    case maybeHandle of
         Just ph -> void $ waitForProcess ph
         Nothing -> pure ()
 where
  noGitEmailText :: Text
  noGitEmailText =
    "The --use-git-email switch was used, but `abbot cite` could not get your email via `git config`.\n"
      <> "       Abbot needs this information to make 'polite' calls to the Crossref API.\n"
      <> "       See https://github.com/CrossRef/rest-api-doc#etiquette for more information."
  noAbbotEmailText :: Text
  noAbbotEmailText =
    "Please set the ABBOT_EMAIL environment variable to your email before using `abbot cite`, "
      <> "       or use `--use-git-email` to let abbot use your .gitconfig email.\n"
      <> "       Abbot needs this information to make 'polite' calls to the Crossref API.\n"
      <> "       See https://github.com/CrossRef/rest-api-doc#etiquette for more information."
  displayError :: Text -> IO ()
  displayError t = TIO.hPutStrLn stderr t'
    where t' = (setColor "tomato" . setBold $ "error: ") <> setColor "tomato" t
  exitWithError :: Text -> IO ()
  exitWithError t = displayError t >> exitFailure


