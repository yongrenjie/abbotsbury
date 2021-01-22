module Abbot.Commands
  ( module Abbot.Commands
  , Abbot.Commands.Shared.ReplCmd(..)
  , Abbot.Commands.Shared.runReplParser
  ) where

import           Abbot.Commands.List
import           Abbot.Commands.Shared
import           Abbot.Reference

import           Data.IntMap                    ( IntMap )


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
