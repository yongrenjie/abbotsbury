module Commands.Search
  ( runSearch
  ) where

import           Commands.Shared
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Lens.Micro.Platform
import           Reference

prefix :: Text
prefix = "search: "

runSearch :: Args -> CmdInput -> CmdOutput
runSearch args input = do
  let refs    = refsin input
      queries = T.words args -- Argument parsing here is trivial.
  -- If no refs present, or no search terms specified error immediately
  errorOnNoRefs prefix input
  when (null queries) $ throwError (prefix <> "no search terms given")
  -- TODO: do the search
  -- Return basically nothing
  pure $ SCmdOutput (refsin input) Nothing

createHaystack :: Reference -> Text
createHaystack ref = undefined
