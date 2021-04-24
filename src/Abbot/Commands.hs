module Abbot.Commands
  ( module Abbot.Commands
  ) where
-- The other two exports are needed for the Main.hs file.

import           Abbot.Commands.Cite
import           Abbot.Commands.List
import           Abbot.Commands.Open
import           Abbot.Commands.Shared
import           Abbot.Commands.Sort

import           Control.Monad.Except


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
      -- Composed commands.
      Composed _ _ -> throwError "pipes not implemented yet"
