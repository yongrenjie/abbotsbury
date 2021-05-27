module Commands.Shared
  ( module Commands.Shared
  ) where

import           Control.Exception              ( IOException(..)
                                                , catch
                                                )
import           System.Environment
import           System.Process
import           Data.Char
import           Data.Foldable                  ( maximumBy )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Internal.Monad
import           Internal.Style                 ( makeError )
import           Lens.Micro.Platform
import           Reference
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

-- * Data types

-- | Quit and Cd are separate from the rest because we don't want them to be
-- 'pipeable'.  They are dealt with by the main loop.
data AbbotCmd
  = Nop
  | Quit
  | Cd Text
  | Single SingleCmd -- Just one command.
  | Composed SingleCmd AbbotCmd -- Two commands joined by a pipe.
  deriving (Show)

data SingleCmd = SingleCmd
  { cbase :: BaseCommand
  , cargs :: Text
  }
  deriving Show

data BaseCommand = Help | List | Cite | Open | Sort
                 | Add | Delete | Edit | Fetch | New
  deriving (Ord, Eq, Show)

-- | A command takes several sources of input:
--  1) @cwdin@: the current working directory
--  2) @refsin@: the current reference list
--  3) @varin@: 'variable input', i.e. something analogous to stdin piped from a
--  previous command. If there is no previous command or if the previous command
--  returns no input, then this is @Nothing@. This does not *replace* @refsin@
--  (i.e.  it does not modify the reference list), but rather acts as a *filter*
--  on the reference list which the next command may choose to obey (or not).
data CmdInput = CmdInput
  { cwdin  :: FilePath
  , refsin :: IntMap Reference
  , varin  :: Maybe IntSet
  }
  deriving Show

-- | A command can perform several IO actions, which ultimately return either an
-- error message (Left), OR a successful return with SCmdOutput, comprising
-- 1) the final state of the reference list, plus
-- 2) an optional set of refnos (which can be piped to another command, see
-- 'CmdInput')
type CmdOutput = ExceptT Text IO SCmdOutput

data SCmdOutput = SCmdOutput
  { refsout :: IntMap Reference
  , varout  :: Maybe IntSet
  }
  deriving Show

-- | We don't want to hardcode the types of the arguments, because they will be
-- different for each command. Each command, when run, will parse their
-- arguments using a particular parser.
type Args = Text

-- * Argument parsing

-- | Setup for command-line parsing.
type Parser = Parsec Void Text

replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment "--") empty

replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace

runReplParser :: Text -> Either (ParseErrorBundle Text Void) AbbotCmd
runReplParser = runParser pAbbotCmd ""

-- | Parser for the abbotsbury command line.
pAbbotCmd :: Parser AbbotCmd
pAbbotCmd = do
  let pArgs = replLexeme $ takeWhileP (Just "arguments") isPrint
  void space -- consume leading space
  cmd <- replLexeme $ choice
    [ try pComposedCmd
    , try (Single <$> pSingleCmd)
    , Cd <$> (replLexeme (string' "cd") >> pArgs)
    , Quit <$ choice (map string' ["quit", "q", ":q", ":wq"])
    , Nop <$ eof
    ]
  eof
  return cmd

pSingleCmd :: Parser SingleCmd
pSingleCmd = do
  baseCmdText <- replLexeme $ takeWhile1P (Just "alphabetical letter") isAlpha
  base        <- case baseCmdText of
    t | t `elem` ["a", "add"]           -> pure Add
      | t `elem` ["c", "cite"]          -> pure Cite
      | t `elem` ["d", "del", "delete"] -> pure Delete
      | t `elem` ["e", "edit"]          -> pure Edit
      | t `elem` ["f", "fetch"]         -> pure Fetch
      | t `elem` ["h", "help"]          -> pure Help
      | t `elem` ["l", "ls", "list"]    -> pure List
      | t `elem` ["n", "new"]           -> pure New
      | t `elem` ["o", "op", "open"]    -> pure Open
      | t `elem` ["so", "sort"]         -> pure Sort
      | otherwise                       -> fail "command not recognised"
  -- For a single command, the arguments cannot include the character '|',
  -- because it is exclusively used in pipes. We need to have a better way
  -- to deal with this, to be honest.
  args <- replLexeme
    $ takeWhileP (Just "arguments") (\c -> isPrint c && c /= '|')
  pure $ SingleCmd base args

pComposedCmd :: Parser AbbotCmd
pComposedCmd = do
  cmd1 <- replLexeme pSingleCmd
  void $ replLexeme (char '|')
  cmd2 <- replLexeme pAbbotCmd
  pure $ Composed cmd1 cmd2

-- | In the REPL, refnos can be specified either using the word 'last' (which
-- is resolved to the most recently opened ref), or as a set of integers.
data Refnos = Last
            | SetOf IntSet

-- | Parse a series of reference numbers. The format is:
-- <NumRange> = <NumLit>-<NumLit>
-- <Num> = <NumLit> | <NumRange>
-- <Refnos> = 0 or more <Num>, separated by commas and/or spaces
pRefnos :: Parser Refnos
pRefnos = pLast <|> pSetOf
 where
  pLast :: Parser Refnos
  pLast  = Last <$ (string' "last" <* (eof <|> space1))
  pSetOf :: Parser Refnos
  pSetOf = SetOf . IS.unions <$> many (pNum <* pSeparator)
  pNum, pNumLit, pNumRange :: Parser IntSet
  pNum      = try pNumRange <|> pNumLit
  pNumLit   = IS.singleton <$> L.decimal -- Parse a number literal
  pNumRange = do                 -- Parse a range of numbers `m-n`
    m <- L.decimal
    void $ char '-'
    n <- L.decimal
    pure $ IS.fromDistinctAscList [m .. n]
  pSeparator :: Parser ()
  pSeparator = void $ takeWhileP Nothing (\c -> isSpace c || c == ',')

resolveRefnosWith :: IntMap Reference -> Refnos -> IntSet
resolveRefnosWith refs rnos = if IM.null refs
  then IS.empty
  else case rnos of
    Last    -> IS.singleton . fst $ maximumBy mostRecent (IM.assocs refs)
    SetOf s -> s
  where
    mostRecent :: (Int, Reference) -> (Int, Reference) -> Ordering
    mostRecent (_, r1) (_, r2) = comparing (^. timeOpened) r1 r2

-- | Parse a series of objects of type @a@, which are represented by (one or
-- more) @Text@ values. If there is a default value, it can be passed as the
-- second parameter, otherwise pass @Nothing@.
pFormats :: forall a . Ord a => Map Text a -> Maybe a -> Parser (Set a)
pFormats abbrevs defval = do
  let parsers :: [Parser a]
      parsers = map (\(k, v) -> v <$ try (string' k))
                    (sortOn (Down . fst) $ M.assocs abbrevs)
  fs <- many $ choice parsers
  case fs of
    -- No formats parsed; look at the default value to see if we should return
    -- something
    [] -> pure $ maybe S.empty S.singleton defval
    -- Formats parsed; convert it to a set and return it
    _  -> pure $ S.fromList fs

-- | Parse a single object of type @a@, which is represented by (one or more)
-- @Text@ values. If there is a default value, it can be passed as the second
-- parameter, otherwise pass @Nothing@.
pOneFormat :: forall a . Map Text a -> Maybe a -> Parser a
pOneFormat abbrevs defval =
  let formatParsers :: [Parser a]
      formatParsers = map (\(k, v) -> v <$ try (string' k))
                          (sortOn (Down . fst) $ M.assocs abbrevs)
      -- in this binding, one pure is for [], one for Parser
      defaultParser :: [Parser a]
      defaultParser = maybe [] (pure . pure) defval
  in  choice (formatParsers ++ defaultParser)

-- | Parse a single object of type @a@, which is represented by (one or more)
-- @Text@ values. If there is a default value, it can be passed as the second
-- parameter, otherwise pass @Nothing@. This code duplication isn't great, but
-- I'm too lazy to fix it for now.
pOneFormatCaseSens :: forall a . Map Text a -> Maybe a -> Parser a
pOneFormatCaseSens abbrevs defval =
  let formatParsers :: [Parser a]
      formatParsers = map (\(k, v) -> v <$ try (string k))
                          (sortOn (Down . fst) $ M.assocs abbrevs)
      defaultParser :: [Parser a]
      defaultParser = maybe [] (pure . pure) defval
  in  choice (formatParsers ++ defaultParser)

-- | Run a parser and @throwError@ if it doesn't parse nicely. Otherwise return
-- the result.
parseInCommand
  ::
  -- | The parser to run.
     Parser r
  ->
  -- | The text to pass to the parser.
     Text
  ->
  -- | The name of the command (plus a colon and space) for error reporting.
     Text
  -> ExceptT Text IO r
parseInCommand parser args prefix = case parse (parser <* eof) "" args of
  Left  bundle -> throwError $ prefix <> T.pack (errorBundlePretty bundle)
  Right x      -> pure x

-- * Assorted helper functions

-- | Make a @Text@ containing a comma-separated list of refnos. Useful for error
-- messages.
intercalateCommas :: IntSet -> Text
intercalateCommas = T.intercalate "," . map (T.pack . show) . IS.toList

-- | A handy wrapper.
printError :: Text -> ExceptT Text IO ()
printError = liftIO . printErrorIO

-- | If you don't want ExceptT.
printErrorIO :: Text -> IO ()
printErrorIO = TIO.putStrLn . makeError

-- | A monadic version of 'either' from base.
mEither :: Monad m => Either a b -> (a -> m c) -> (b -> m c) -> m c
mEither (Left  a) f1 _  = f1 a
mEither (Right b) _  f2 = f2 b

refnoT :: Int -> Text
refnoT i = "refno " <> T.pack (show i) <> ": "

-- | Figure out whether to get the refnos from the arguments, or from the varin.
getActiveRefnos
  :: Text -- ^ A prefix to add to any error messages thrown.
  -> IntSet -- ^ The refnos parsed from the arguments.
  -> CmdInput -- ^ The input passed to the command.
  -> ExceptT Text IO IntSet
getActiveRefnos prefix argsRefnos input =
  case (varin input, IS.null argsRefnos) of
    (Nothing     , True ) -> throwError (prefix <> "no references selected")
    (Nothing     , False) -> pure argsRefnos
    (Just vRefnos, True ) -> pure vRefnos
    (Just vRefnos, False) ->
      throwError (prefix <> "cannot specify refnos and a pipe simultaneously")

-- | Error if the reference list is empty.
errorOnNoRefs
  :: Text -- ^ A prefix to add to any error messages thrown.
  -> CmdInput -- ^ The input passed to the command.
  -> ExceptT Text IO ()
errorOnNoRefs prefix input = when
  (IM.null $ refsin input)
  (throwError $ prefix <> "no references available")

-- | Error if some refnos don't exist in the reference list.
errorOnInvalidRefnos
  :: Text -- ^ A prefix to add to any error messages thrown.
  -> IntSet -- ^ The set of supposed refnos to act on.
  -> CmdInput -- ^ The input passed to the command.
  -> ExceptT Text IO ()
errorOnInvalidRefnos prefix refnos input = do
  let badRefnos = refnos IS.\\ IM.keysSet (refsin input)
  unless
    (IS.null badRefnos)
    (throwError
      (prefix <> "reference(s) " <> intercalateCommas badRefnos <> " not found")
    )

-- | Attempt to get the email to use for HTTP requests.
getUserEmail :: Text -> ExceptT Text IO Text
getUserEmail prefix = do
  -- Try to get the environment variable.
  maybeEnvvar <- liftIO $ lookupEnv "ABBOT_EMAIL"
  -- Try to get it from the gitconfig.
  let getGitEmail =
        Just <$> readProcess "git" ["config", "--get", "user.email"] []
  maybeGit <- liftIO $ catch getGitEmail (\(e :: IOException) -> pure Nothing)
  case (maybeEnvvar, maybeGit) of
    (Just e , _      ) -> pure $ T.strip $ T.pack e
    (Nothing, Just e') -> pure $ T.strip $ T.pack e'
    _                  -> throwError
      (  prefix
      <> "no email was specified. "
      <> "Please set either the ABBOT_EMAIL environment variable, "
      <> "or set an email in your .gitconfig file."
      )

-- | This regex check is VERY basic, I don't intend for it to be very selective.
-- It's just to weed out extremely obvious mistakes.
isValidDoi :: DOI -> Bool
isValidDoi = isJust . parseMaybe pDoi
  where
    pDoi :: Parser ()
    pDoi = do
      void $ string "10."
      numbers <- takeWhileP Nothing isDigit
      guard $ 4 <= T.length numbers && T.length numbers <= 9
      void $ char '/'
      void $ takeWhileP Nothing (not . isSpace)
      eof
      pure ()
