module Abbot.Commands.Shared
  ( module Abbot.Commands.Shared
  ) where

import           Abbot.Reference

import           Control.Monad.Except
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
-- import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


-- | We don't want to hardcode the types of the arguments, because they will
-- be different for each command. Each command, when run, will parse their
-- arguments using a particular parser.
data ReplCmd = Nop | Quit | Help | Cd | List | Cite | Open | Sort
             deriving (Ord, Eq, Show)
type ReplArgs = Text


-- | Setup for command-line parsing.
type Parser = Parsec Void Text
replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment "--") empty
replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace
runReplParser
  :: Text -> Either (ParseErrorBundle Text Void) (ReplCmd, ReplArgs)
runReplParser = runParser pRepl ""


-- | Parser for the abbotsbury command line.
-- TODO: This has slightly inconsistent behaviour in that spaces are not
-- required *after* the first command. This is OK for commands like `cite`
-- and `list`, but is quite odd for `help`: for example, both `help quit`
-- and `helpquit` are parsed as (Help, "quit").
pRepl :: Parser (ReplCmd, ReplArgs)
pRepl = do
  void $ replLexeme space        -- consume leading space
  cmd <- replLexeme $ choice
    [ Nop <$ eof
    , Cd <$ string' "cd"
    , Help <$ string' "help"
    , Quit <$ string' "quit"
    , List <$ choice (map string' ["list", "ls", "l"])
    , Open <$ choice (map string' ["open", "op", "o"])
    , Sort <$ choice (map string' ["sort", "so"])
    ]
  args <- replLexeme $ takeWhileP Nothing isPrint
  eof
  return (cmd, args)


-- | A command can perform several IO actions, which ultimately return either
-- an error message (Left), OR a successful return with
-- 1) the final state of the reference list, plus
-- 2) an optional set of refnos (which can be piped to another command)
type CmdOutput = ExceptT Text IO (IntMap Reference, Maybe IntSet)


-- | Parse a series of reference numbers. The format is:
-- <NumRange> = <NumLit>-<NumLit>
-- <Num> = <NumLit> | <NumRange>
-- <Refnos> = 0 or more <Num>, separated by commas and/or spaces
pRefnos :: Parser IntSet
pRefnos = IS.unions <$> many (pNum <* pSeparator)
 where
  pNum, pNumLit, pNumRange :: Parser IntSet
  pNum      = try pNumRange <|> pNumLit
  pNumLit   = IS.singleton <$> L.decimal  -- Parse a number literal
  pNumRange = do                        -- Parse a range of numbers `m-n`
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
      parsers = map (\(k, v) -> v <$ try (string' k)) (sortOn (Down . fst) $ M.assocs abbrevs)
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
      formatParsers = map (\(k, v) -> v <$ try (string' k)) (sortOn (Down . fst) $ M.assocs abbrevs)
      defaultParser :: [Parser a]
      defaultParser = maybe [] (pure . pure) defval  -- one pure for [], one for Parser
      in choice (formatParsers ++ defaultParser)


-- | Because the help command requires runReplParser itself, we can't stick it
-- in a different module (that would lead to a cyclic import).
runHelp :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runHelp args _ refs = case runReplParser args of
  Left  _        -> throwError ("help: command '" <> args <> "' not recognised")
  Right (cmd, _) -> do
    liftIO $ TIO.putStrLn (getHelpText cmd)
    pure (refs, Nothing)


-- | Returns the help text for a particular command.
getHelpText :: ReplCmd -> Text
getHelpText = \case
  Nop  -> "Welcome to abbotsbury! The help hasn't been written yet."
  Quit -> "Exit the program."
  Help -> "Print help."
  Cd   -> "Change the working directory."
  Cite -> "Cite some references."
  Open -> "Open some references."
  List -> "List selected or all references."
  Sort -> "Sort references."
