{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Abbot.Commands
  ( module Abbot.Commands
  ) where

import           Data.Char
import           Data.Functor                   ( void )
import           Data.IntSet                    ( IntSet )
-- import qualified Data.IntSet                   as IS
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
data ReplCmd = Nop | Quit | Help | Cd | Cite
             deriving (Eq, Show)
type ReplArgs = Text


-- | Command-line parsing

type Parser = Parsec Void Text

-- | It's a Haskell port, so of course we need to make the comment string be
-- '--' instead of '#' in cygnet. In fact, it also saves us some trouble
-- because '--' feels far less likely to appear in something like a URL or a
-- filename.
replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment "--") empty
replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace

runReplParser
  :: Text -> Either (ParseErrorBundle Text Void) (ReplCmd, ReplArgs)
runReplParser = runParser pRepl ""

pRepl :: Parser (ReplCmd, ReplArgs)
pRepl = do
  void $ replLexeme space
  cmd <- replLexeme $ choice
    [ Nop <$ eof
    , Cd <$ string' "cd"
    , Help <$ string' "help"
    , Quit <$ string' "quit"
    ]
  args <- replLexeme $ takeWhileP Nothing isPrint
  eof
  return (cmd, args)


-- | A command can perform several IO actions, which ultimately return either
--   an error message (Left), OR a successful return with
--   1) the final state of the reference list, plus
--   2) an optional set of refnos (which can be piped to another command)
type CmdOutput = IO (Either Text ([Reference], Maybe IntSet))
type Reference = Text


-- | Run a command. This is just a helper function which delegates to the
-- individual command runners.
runCommand
  :: ReplCmd         -- The command to be run.
  -> ReplArgs        -- The arguments passed to the command.
  -> [Reference]     -- The current state of the reference list.
  -> CmdOutput       -- The output of the command.
runCommand Help = runHelp
runCommand _    = undefined

-- | HELP
runHelp :: ReplArgs -> [Reference] -> CmdOutput
runHelp args refs = case runReplParser args of
  Left _ -> pure $ Left $ "help: command '" <> args <> "' not recognised"
  Right (cmd, _) ->
    TIO.putStrLn (getHelpText cmd) >> pure (Right (refs, Nothing))

getHelpText :: ReplCmd -> Text
getHelpText Nop  = "Welcome to abbotsbury! The help hasn't been written yet."
getHelpText Quit = "Exit the program."
getHelpText Help = "Print help."
getHelpText Cd   = "Change the working directory."
getHelpText Cite = "Cite some references."
