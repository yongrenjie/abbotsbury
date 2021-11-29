module Commands
  ( module Commands
  ) where

import           Commands.Add
import           Commands.Addpdf
import           Commands.Cite
import           Commands.Delete
import           Commands.Deletepdf
import           Commands.Edit
import           Commands.Fetch
import           Commands.Help
import           Commands.New
import           Commands.Open
import           Commands.Search
import           Commands.Shared
import           Commands.Sort
import           Internal.Monad

-- | Execute a command given an input.
-- Note that this only handles effects *outside* the main loop, i.e.
-- Nop, Quit, and Cd should all do nothing.
runCmdWith :: AbbotCmd -> CmdInput -> CmdOutput
runCmdWith cmd input =
  let nop = SCmdOutput (refsin input) Nothing
  in  case cmd of
        Nop                          -> pure nop
        Quit                         -> pure nop
        (Cd _)                       -> pure nop
        -- Just one command...
        Single (SingleCmd base args) -> case base of
          Add       -> runAdd args input
          Addpdf    -> runAddpdf args input
          Cite      -> runCite args input
          Delete    -> runDelete args input
          Deletepdf -> runDeletepdf args input
          Edit      -> runEdit args input
          Fetch     -> runFetch args input
          Help      -> runHelp args >> pure nop
          New       -> runNew args input
          Open      -> runOpen args input
          Search    -> runSearch args input
          Sort      -> runSort args input
        -- Composed commands.
        Composed single1 other2 -> do
          SCmdOutput refs2 var2 <- runCmdWith (Single single1) input
          let input2 = CmdInput (cwdin input) refs2 var2
          runCmdWith other2 input2
