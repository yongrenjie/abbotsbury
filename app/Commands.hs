module Commands
  ( module Commands
  ) where

import           Commands.Add
import           Commands.Cite
import           Commands.Delete
import           Commands.Edit
import           Commands.Fetch
import           Commands.Help
import           Commands.List
import           Commands.Open
import           Commands.Shared
import           Commands.Sort
import           Internal.Monad

-- | Execute a command given an input.
-- Note that this only handles effects *outside* the main loop, i.e.
-- Nop, Quit, and Cd should all do nothing.
runCmdWith :: ReplCmd -> CmdInput -> CmdOutput
runCmdWith cmd input =
  let nop = SCmdOutput (refsin input) Nothing
  in  case cmd of
        Nop                         -> pure nop
        Quit                        -> pure nop
        (Cd _)                      -> pure nop
        -- Just one command...
        Single (AbbotCmd base args) -> case base of
          Help   -> runHelp args >> pure nop
          List   -> runList args input
          Cite   -> runCite args input
          Open   -> runOpen args input
          Sort   -> runSort args input
          Add    -> runAdd args input
          Delete -> runDelete args input
          Edit   -> runEdit args input
          Fetch  -> runFetch args input
        -- Composed commands.
        Composed cmd1 cmd2 -> do
          SCmdOutput refs2 var2 <- runCmdWith (Single cmd1) input
          let input2 = CmdInput (cwdin input) refs2 var2
          runCmdWith (Single cmd2) input2
