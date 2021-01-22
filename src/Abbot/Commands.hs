module Abbot.Commands
  ( module Abbot.Commands
  ) where

import           Abbotsbury

import           Data.Char
import           Data.Functor                   ( void )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Lens.Micro.Platform
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


-- | Run a command. This is just a helper function which delegates to the
-- individual command runners.
runCommand
  :: ReplCmd          -- The command to be run.
  -> ReplArgs         -- The arguments passed to the command.
  -> IntMap Reference -- The current state of the reference list.
  -> CmdOutput        -- The output of the command.
runCommand Help = runHelp
runCommand List = runList
runCommand _    = undefined


-- | Runs the `help` command.
runHelp :: ReplArgs -> IntMap Reference -> CmdOutput
runHelp args refs = case runReplParser args of
  Left _ -> cmdErr ("help: command '" <> args <> "' not recognised")
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


-- | LIST
runList :: ReplArgs -> IntMap Reference -> CmdOutput
runList args refs = if IM.null refs
  then cmdErrS "list: no references found"
  else
    let parsedArgs = parse pRefnos "" args
        numRefs    = fst $ IM.findMax refs
    in  case parsedArgs of
          Left  bundle -> cmdErrS ("list: " ++ errorBundlePretty bundle)  -- parse error
          Right refnos -> if IS.null refnos
            then do
              prettyPrintRefHead
              mapM_ (prettyPrintRef refs) [1 .. numRefs]
              pure $ Right (refs, Just $ IS.fromList [1 .. numRefs])
            else case IS.lookupGT numRefs refnos <|> IS.lookupLT 1 refnos of
              Just x ->
                cmdErrS ("list: reference " <> show x <> " is out of bounds")
              Nothing -> do
                prettyPrintRefHead
                mapM_ (prettyPrintRef refs) (IS.toList refnos)
                pure $ Right (refs, Just refnos)

-- | Pretty-print the header of the reference list.
prettyPrintRefHead :: IO ()
prettyPrintRefHead = putStrLn "REFERENCES\n=========="

-- | Pretty-print a specific reference from the reference list, given a
-- (1-indexed) number. The output is (well, should be) based on the terminal
-- width.
prettyPrintRef :: IntMap Reference -> Int -> IO ()
prettyPrintRef refs index = do
  TIO.putStr (T.pack (show index) <> ": ")
  TIO.putStrLn (refs ^. ix index . title)

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
