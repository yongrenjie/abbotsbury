module Commands.Help (runHelp) where

import           Commands.Shared
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad

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
      "That's a pipe..."
 where
  getHelpText :: BaseCommand -> Text
  getHelpText = \case
    Help -> "Print help."
    Cite -> "Cite some references."
    Open -> "Open some references."
    List -> "List selected or all references."
    Sort -> "Sort references."
