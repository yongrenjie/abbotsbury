{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Abbot.Commands
  ( module Abbot.Commands
  ) where

import           Data.Char
import           Data.Functor                   ( void )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


data ReplCmd = Nop | Quit | Help | Cd
type ReplArgs = Text


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

-- | The output of a command is either an error message (Left), OR
--   1) an optional set of refnos (which can be piped to another command), plus
--   2) a list of IO actions to be performed by the main loop
type CmdOutput = Either Text (Maybe IntSet, [IO ()])
type Reference = Text

-- | Return a result which has no effect except for (eventually) being printed
-- by the main loop.
purePrint :: Text -> CmdOutput
purePrint s = Right (Nothing, [TIO.putStrLn s])

-- | Run a command.
runCommand :: ReplCmd       -- The command to be run.
           -> ReplArgs      -- The arguments passed to the command.
           -> [Reference]   -- The current state of the reference list.
           -> CmdOutput     -- The output of the command.
runCommand Help args _ = runHelp args
runCommand _    _    _ = undefined


-- HELP

runHelp :: ReplArgs -> CmdOutput
runHelp ""   = purePrint genericHelp
runHelp args = case runReplParser args of
  Left  _        -> Left "command not recognised"
  Right (cmd, _) -> purePrint $ getHelpText cmd


genericHelp :: Text
genericHelp = "Welcome to abbotsbury! The help hasn't been written yet."
getHelpText :: ReplCmd -> Text
getHelpText Quit = "Exit the program."
getHelpText Help = "Print help."
getHelpText Cd   = "Change the working directory."
