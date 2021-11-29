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
  { _curDir     :: FilePath -- Current working directory ('wd').
  , _oldDir     :: FilePath -- The last wd. Analogous to bash $OLDPWD.
  , _dirChanged :: Bool -- Did last command change wd?
  , _verbosity :: AbbotVerbosity  -- Verbosity level
  , _references :: IntMap Reference -- The references.
  , _refsWidget :: [Widget RName] -- Pretty-printed references
  , _refsCurrent :: Int     -- Currently active reference
  , _status :: Text         -- Status to display in the bottom bar
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
  refsWidget <- prettify' curDir (Just 5) refsCurrent (IM.assocs refs)
  pure $ AbbotState { _curDir = curDir
                    , _oldDir = oldDir
                    , _dirChanged = dirChanged
                    , _verbosity = Quiet
                    , _references = refs
                    , _refsWidget = refsWidget
                    , _refsCurrent = refsCurrent
                    , _status = "Press a key or something..." }

app :: App AbbotState e RName
app = App
  { appDraw         = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap
                        mempty
                        [ ("bold"       , withStyle currentAttr bold)
                        , ("selectedRef", rgbColor 0 55 215 `on` rgbColor 215 255 255)
                        ]
  }


-- Recalculates the refWidget component of the state. 
updateState :: AbbotState -> IO AbbotState
updateState ast = do
  let cwd   = ast ^. curDir
      refs  = ast ^. references
      refsN = ast ^. refsCurrent
  refsWidget <- prettify' cwd (Just 5) refsN (IM.assocs refs)
  pure $ ast { _refsWidget = refsWidget }


-- Render the TUI.
draw :: AbbotState -> [Widget RName]
draw ast =
  let refsW = head $ ast ^. refsWidget
      cmdW  = border . padRight Max . txt $ ast ^. status
  in  [vBox [refsW, cmdW]]

-- Handle events.
handleEvent
  :: AbbotState -> BrickEvent RName e -> EventM RName (Next AbbotState)
handleEvent ast e = do
  vty <- getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $ liftIO $ setMode output Mouse True
  case e of
    VtyEvent vtye -> case vtye of
      -- q: quit
      EvKey (KChar 'q') []      -> halt ast
      -- <C-L>: redraw
      EvKey (KChar 'l') [MCtrl] -> continue ast
      -- j: move down
      EvKey (KChar 'j') []      -> do
        let refs = ast ^. references
            curr = ast ^. refsCurrent
        case IM.lookupGT curr refs of
          Just (next, _) -> do
            ast' <- liftIO $ updateState $ ast { _refsCurrent = next }
            continue $ ast' {_status = T.pack . show $ next}
          Nothing        -> continue $ ast  {_status = "Nothing" }
      -- k: move up
      EvKey (KChar 'k') [] -> do
        let refs = ast ^. references
            curr = ast ^. refsCurrent
        case IM.lookupLT curr refs of
          Just (prev, _) -> do
            ast' <- liftIO $ updateState $ ast { _refsCurrent = prev }
            continue $ ast' {_status = T.pack . show $ prev}
          Nothing        -> continue $ ast  {_status = "Nothing" }
      -- Something else: show it in the status window
      other -> do
        let ast' = ast { _status = T.pack . show $ other }
        continue ast'
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


-- old stuff                 
-----------------------------

-- | The main REPL loop of abbot. The `quit` and `cd` functions are implemented
-- here, because they affect the entire state of the program. Other functions
-- are implemented elsewhere.
loop :: MInputT (StateT AbbotState IO) ()
loop =
  let exit = pure ()
      save = do
        refs <- use references
        curD <- use curDir
        unless (null refs) (liftIO $ saveRefs refs curD)
  in  mHandleInterrupt loop $ do
        curD <- use curDir
        dirC <- use dirChanged
        verb <- use verbosity

        -- If the directory was changed, read in new data, then turn off the flag
        when dirC $ do
          newRefs <- liftIO . runExceptT $ readRefs curD
          case newRefs of
            Right nrefs -> do
              references .= nrefs
              unless
                (null nrefs || verb == Quiet)
                (mOutputStrLn
                  ("Read in " <> T.pack (show (length nrefs)) <> " references.")
                )
            Left errmsg -> printErr errmsg
        dirChanged .= False

        -- Show the prompt and get the command
        cwd   <- liftIO $ expandDirectory curD
        input <- prompt verb cwd

        -- Parse and run the command
        case input of
          Nothing      -> save >> exit -- Ctrl-D
          Just cmdArgs -> case runReplParser cmdArgs of
            Left _ ->
              printErr ("command '" <> cmdArgs <> "' not recognised") >> loop
            -- Special commands that we need to handle in main loop
            Right Nop     -> loop
            Right Quit    -> mOutputStrLn "quitting..." >> save >> exit
            Right (Cd fp) -> do
              -- Check for 'cd -'
              newD <- if fp == "-"
                then use oldDir
                else liftIO $ expandDirectory (T.unpack fp)
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
              let cmdInput = CmdInput currentDir currentRefs Nothing
              -- Run the command. If an IOException occurs, display it and
              -- continue the programme.
              cmdOutput <- liftIO $ catch
                (runExceptT (runCmdWith otherCmd cmdInput))
                (\(e :: IOException) -> do
                  TIO.putStrLn . makeError . T.pack . show $ e
                  pure $ Right (SCmdOutput currentRefs Nothing)
                )
              case cmdOutput of
                Left  err                    -> printErr err >> loop
                Right (SCmdOutput newRefs _) -> do
                  references .= newRefs
                  save
                  loop

-- | Generates the prompt for the main loop.
prompt
  :: AbbotVerbosity -> FilePath -> MInputT (StateT AbbotState IO) (Maybe Text)
prompt verb fp = mGetInputLine promptText
 where
  promptText
    | verb == Quiet = ""
    | otherwise = mconcat
      [ setColor "plum" $ "(" <> T.pack fp <> ") "
      , setColor "hotpink" . setBold . setItalic $ "peep > "
      ]

-- | Prints an error in the main loop.
printErr :: Text -> MInputT (StateT AbbotState IO) ()
printErr = mOutputStrLn . makeError


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


