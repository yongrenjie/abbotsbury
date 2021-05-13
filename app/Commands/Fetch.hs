module Commands.Fetch
  ( runFetch
  ) where

import           Commands.Shared
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Lens.Micro.Platform
import           Reference

prefix :: Text
prefix = "fetch: "

runFetch :: Args -> CmdInput -> CmdOutput
runFetch args input = do
  throwError (prefix <> "not implemented yet")
