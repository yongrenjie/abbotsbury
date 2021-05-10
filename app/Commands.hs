module Commands
  ( module Commands
  ) where
-- The other two exports are needed for the Main.hs file.


import           Commands.Add
import           Commands.Cite
import           Commands.List
import           Commands.Open
import           Commands.Shared
import           Commands.Sort
import           Control.Monad.Except           ( throwError )


-- | Execute a command given an input.
-- Note that this only handles effects *outside* the main loop, i.e.
-- Nop, Quit, and Cd should all do nothing.
runCmdWith :: ReplCmd -> CmdInput -> CmdOutput
runCmdWith cmd input = 
  let nop = SCmdOutput (refsin input) Nothing
  in case cmd of
      Nop -> pure nop
      Quit -> pure nop
      (Cd _) -> pure nop
      -- Just one command...
      Single (AbbotCmd base args) -> case base of
        Help -> runHelp args >> pure nop
        List -> runList args input
        Cite -> runCite args input
        Open -> runOpen args input
        Sort -> runSort args input
        Add  -> runAdd  args input
      -- Composed commands.
      Composed cmd1 cmd2 -> do
        SCmdOutput refs2 var2 <- runCmdWith (Single cmd1) input
        let input2 = CmdInput (cwdin input) refs2 var2
        runCmdWith (Single cmd2) input2
