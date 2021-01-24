module Abbot.Commands.Shared
  ( module Abbot.Commands.Shared
  ) where

import           Abbot.Reference

import           Data.Char
import           Data.Functor                   ( void )
import           Data.IntMap                    ( IntMap )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


-- | We don't want to hardcode the types of the arguments, because they will
-- be different for each command. Each command, when run, will parse their
-- arguments using a particular parser.
data ReplCmd = Nop | Quit | Help | Cd | List | Cite
             deriving (Eq, Show)
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
    ]
  args <- replLexeme $ takeWhileP Nothing isPrint
  eof
  return (cmd, args)


-- | A command can perform several IO actions, which ultimately return either
--   an error message (Left), OR a successful return with
--   1) the final state of the reference list, plus
--   2) an optional set of refnos (which can be piped to another command)
type CmdOutput = IO (Either Text (IntMap Reference, Maybe IntSet))


-- | Shortcuts to return an error
cmdErr :: Text -> CmdOutput
cmdErr = pure . Left
cmdErrS :: String -> CmdOutput
cmdErrS = pure . Left . T.pack


-- | Parse a series of reference numbers. The format is:
-- <NumRange> = <NumLit>-<NumLit>
-- <Num> = <NumLit> | <NumRange>
-- <Refnos> = 0 or more <Num>, separated by commas and/or spaces
pRefnos :: Parser IntSet
pRefnos = IS.unions <$> many (pNum <* pSeparator) <* eof
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


-- | Because the help command requires runReplParser itself, we can't stick it
-- in a different module (that would lead to a cyclic import).
runHelp :: ReplArgs -> FilePath -> IntMap Reference -> CmdOutput
runHelp args _ refs = case runReplParser args of
  Left  _        -> cmdErr ("help: command '" <> args <> "' not recognised")
  Right (cmd, _) -> do
    TIO.putStrLn (getHelpText cmd)
    pure (Right (refs, Nothing))

-- | Returns the help text for a particular command.
getHelpText :: ReplCmd -> Text
getHelpText = \case
  Nop  -> "Welcome to abbotsbury! The help hasn't been written yet."
  Quit -> "Exit the program."
  Help -> "Print help."
  Cd   -> "Change the working directory."
  Cite -> "Cite some references."
  List -> "List selected or all references."
