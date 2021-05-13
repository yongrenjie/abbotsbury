module Commands.Shared
  ( module Commands.Shared
  ) where

import           Data.Char
import           Data.IntMap                    ( IntMap )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Ord                       ( Down(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Internal.Monad
import           Internal.Style                 ( makeError )
import           Reference
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

-- | Quit and Cd are separate from the rest because we don't want them to be 'pipeable'.
-- They are dealt with by the main loop.
data ReplCmd
  = Nop
  | Quit
  | Cd Text
  | Single AbbotCmd -- Just one command.
  | Composed AbbotCmd AbbotCmd -- Two commands joined by a pipe.
  deriving (Show)

data AbbotCmd = AbbotCmd
  { cbase :: BaseCommand
  , cargs :: Text
  }
  deriving Show

data BaseCommand = Help | List | Cite | Open | Sort | Add | Delete | Edit
  deriving (Ord, Eq, Show)

-- | A command takes several sources of input:
--  1) cwdin: the current working directory
--  2) refsin: the current reference list
--  3) varin: 'variable input', i.e. 'stdin' piped from a previous command. If there is no previous
--  command or if the previous command returns no input, then this is Nothing. This does not
--  *replace* refsin (i.e. it does not modify the reference list), but rather acts as a *filter* on
--  the reference list which the next command may choose to obey (or not).
data CmdInput = CmdInput
  { cwdin  :: FilePath
  , refsin :: IntMap Reference
  , varin  :: Maybe IntSet
  }
  deriving Show

-- | A command can perform several IO actions, which ultimately return either
-- an error message (Left), OR a successful return with SCmdOutput, comprising
-- 1) the final state of the reference list, plus
-- 2) an optional set of refnos (which can be piped to another command, see `CmdInput`)
type CmdOutput = ExceptT Text IO SCmdOutput

data SCmdOutput = SCmdOutput
  { refsout :: IntMap Reference
  , varout  :: Maybe IntSet
  }
  deriving Show

-- | We don't want to hardcode the types of the arguments, because they will
-- be different for each command. Each command, when run, will parse their
-- arguments using a particular parser.
type Args = Text

-- | Setup for command-line parsing.
type Parser = Parsec Void Text

replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment "--") empty

replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace

runReplParser :: Text -> Either (ParseErrorBundle Text Void) ReplCmd
runReplParser = runParser pRepl ""

-- | Parser for the abbotsbury command line.
pRepl :: Parser ReplCmd
pRepl = do
  let pArgs = replLexeme $ takeWhileP (Just "arguments") isPrint
  void space -- consume leading space
  -- It's very awkward that 'cd' must come before the rest, because otherwise
  -- the 'c' is parsed as being a cite command.
  cmd <- replLexeme $ choice
    [ Cd <$> (replLexeme (string' "cd") >> pArgs)
    , try pComposedCmd
    , Single <$> pSingleCmd
    , Quit <$ choice (map string' ["quit", "q"])
    , Nop <$ eof
    ]
  eof
  return cmd

pSingleCmd :: Parser AbbotCmd
pSingleCmd = do
  baseCmdText <- replLexeme $ takeWhile1P (Just "alphabetical letter") isAlpha
  base        <- case baseCmdText of
    t | t `elem` ["h", "help"]          -> pure Help
      | t `elem` ["l", "ls", "list"]    -> pure List
      | t `elem` ["so", "sort"]         -> pure Sort
      | t `elem` ["c", "cite"]          -> pure Cite
      | t `elem` ["a", "add"]           -> pure Add
      | t `elem` ["d", "del", "delete"] -> pure Delete
      | t `elem` ["e", "edit"]          -> pure Edit
      | otherwise                       -> fail "command not recognised"
  -- For a single command, the arguments cannot include the character '|',
  -- because it is exclusively used in pipes. We need to have a better way
  -- to deal with this, to be honest.
  args <- replLexeme
    $ takeWhileP (Just "arguments") (\c -> isPrint c && c /= '|')
  pure $ AbbotCmd base args

pComposedCmd :: Parser ReplCmd
pComposedCmd = do
  cmd1 <- replLexeme pSingleCmd
  void $ replLexeme (char '|')
  cmd2 <- replLexeme pSingleCmd
  pure $ Composed cmd1 cmd2

-- | Parse a series of reference numbers. The format is:
-- <NumRange> = <NumLit>-<NumLit>
-- <Num> = <NumLit> | <NumRange>
-- <Refnos> = 0 or more <Num>, separated by commas and/or spaces
pRefnos :: Parser IntSet
pRefnos = IS.unions <$> many (pNum <* pSeparator)
 where
  pNum, pNumLit, pNumRange :: Parser IntSet
  pNum      = try pNumRange <|> pNumLit
  pNumLit   = IS.singleton <$> L.decimal -- Parse a number literal
  pNumRange = do
    -- Parse a range of numbers `m-n`
    m <- L.decimal
    void $ char '-'
    n <- L.decimal
    pure $ IS.fromDistinctAscList [m .. n]
  pSeparator :: Parser ()
  pSeparator = void $ takeWhileP Nothing (\c -> isSpace c || c == ',')

-- | Parse a series of objects of type a, which are represented by (one or more)
-- Text values. If there is a default value, it can be passed as the second parameter,
-- otherwise pass Nothing.
pFormats :: forall a . Ord a => Map Text a -> Maybe a -> Parser (Set a)
pFormats abbrevs defval = do
  let parsers :: [Parser a]
      parsers = map (\(k, v) -> v <$ try (string' k))
                    (sortOn (Down . fst) $ M.assocs abbrevs)
  fs <- many $ choice parsers
  case fs of
    -- No formats parsed; look at the default value to see if we should return something
    [] -> pure $ maybe S.empty S.singleton defval
    -- Formats parsed; convert it to a set and return it
    _  -> pure $ S.fromList fs

-- | Parse a single object of type a, which is represented by (one or more)
-- Text values. If there is a default value, it can be passed as the second parameter,
-- otherwise pass Nothing.
pOneFormat :: forall a . Map Text a -> Maybe a -> Parser a
pOneFormat abbrevs defval =
  let formatParsers :: [Parser a]
      formatParsers = map (\(k, v) -> v <$ try (string' k))
                          (sortOn (Down . fst) $ M.assocs abbrevs)
      defaultParser :: [Parser a]
      defaultParser = maybe [] (pure . pure) defval -- one pure for [], one for Parser
  in  choice (formatParsers ++ defaultParser)

-- | Parse a single object of type a, which is represented by (one or more)
-- Text values. If there is a default value, it can be passed as the second parameter,
-- otherwise pass Nothing.
-- This code duplication isn't great, but I'm too lazy to fix it for now.
pOneFormatCaseSens :: forall a . Map Text a -> Maybe a -> Parser a
pOneFormatCaseSens abbrevs defval =
  let formatParsers :: [Parser a]
      formatParsers = map (\(k, v) -> v <$ try (string k))
                          (sortOn (Down . fst) $ M.assocs abbrevs)
      defaultParser :: [Parser a]
      defaultParser = maybe [] (pure . pure) defval -- one pure for [], one for Parser
  in  choice (formatParsers ++ defaultParser)

-- | Run a parser and throwError if it doesn't parse nicely. Otherwise return the result. This just
-- simplifies the code in the runCmd family of functions.
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

-- | Because the help command requires runReplParser itself, we can't stick it
-- in a different module (that would lead to a cyclic import).
runHelp :: Text -> ExceptT Text IO ()
runHelp args = case runReplParser args of
  Left  _    -> throwError ("help: command '" <> args <> "' not recognised")
  Right repl -> liftIO $ case repl of
    Nop ->
      TIO.putStrLn "Welcome to abbotsbury! The help hasn't been written yet."
    Quit           -> TIO.putStrLn "Exit the programme."
    (Cd     _    ) -> TIO.putStrLn "Change the working directory."
    (Single acmd ) -> TIO.putStrLn (getHelpText $ cbase acmd)
    (Composed _ _) -> TIO.putStrLn
      "That's a composed command, but it hasn't been implemented yet!"
 where
  getHelpText :: BaseCommand -> Text
  getHelpText = \case
    Help -> "Print help."
    Cite -> "Cite some references."
    Open -> "Open some references."
    List -> "List selected or all references."
    Sort -> "Sort references."

-- | Make a Text containing a comma-separated list of refnos. Useful for error messages.
intercalateCommas :: IntSet -> Text
intercalateCommas = T.intercalate "," . map (T.pack . show) . IS.toList

-- | A handy wrapper.
printError :: Text -> ExceptT Text IO ()
printError text = liftIO $ TIO.putStrLn (makeError text)
