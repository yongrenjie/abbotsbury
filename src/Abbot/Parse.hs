module Abbot.Parse
  ( module Abbot.Parse
  ) where

import           Data.Functor                   ( void )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


data ReplCommand = Quit
                 | Help (Maybe ReplCommand)  -- if Nothing, prints general help
                 | Cd FilePath

type Parser = Parsec Void String

replSpace :: Parser ()
replSpace = L.space space1 (L.skipLineComment "#") empty
replLexeme :: Parser a -> Parser a
replLexeme = L.lexeme replSpace

runReplParser :: String -> Either (ParseErrorBundle String Void) ReplCommand
runReplParser = runParser pRepl ""

pRepl :: Parser ReplCommand
pRepl = void space *> replLexeme pCmd <* eof
  where
    pCmd = choice [ pCd
                  , pQuit
                  ]

pCd :: Parser ReplCommand
pCd = do
  void $ replLexeme $ string' "cd"
  fp <- many (punctuationChar <|> alphaNumChar)
  return $ Cd fp

pQuit :: Parser ReplCommand
pQuit = do
  void $ optional $ char ':'
  void $ choice $ map string' ["quit", "qui", "qu", "q"]
  return Quit
