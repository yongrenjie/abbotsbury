{-# LANGUAGE LambdaCase #-}

module Abbot.Commands
  ( module Abbot.Commands
  ) where

import           Data.Char
import           Data.Functor                   ( void )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


data ReplCmd = Quit
                 | Help
                 | Cd
type ReplArgs = String


type Parser = Parsec Void String

replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment " #") empty
replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace

runReplParser
  :: String -> Either (ParseErrorBundle String Void) (ReplCmd, ReplArgs)
runReplParser = runParser pRepl ""

pRepl :: Parser (ReplCmd, ReplArgs)
pRepl = do
  void space
  cmd <- replLexeme $ choice
    [Cd <$ string' "cd", Help <$ string' "help", Quit <$ string' "quit"]
  args <- replLexeme $ takeWhileP Nothing isPrint
  eof
  return (cmd, args)

-- | The output of a command is either an error message (Left), OR
--   1) an optional set of refnos (which can be piped to another command), plus
--   2) a list of IO actions to be performed by the main loop
type CmdOutput = Either String (Maybe IntSet, [IO ()])
type Reference = String

-- | Return a result which has no effect except for (eventually) being printed
-- by the main loop.
purePrint :: String -> CmdOutput
purePrint s = Right (Nothing, [putStrLn s])

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
  Right (cmd, _) -> purePrint $ getHelpString cmd


genericHelp :: String
genericHelp = "Welcome to abbotsbury! The help hasn't been written yet."
getHelpString :: ReplCmd -> String
getHelpString Quit = "Exit the program."
getHelpString Help = "Print help."
getHelpString Cd   = "Change the working directory."
