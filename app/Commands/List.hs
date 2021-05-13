module Commands.List
  ( runList
  ) where

import           Commands.Shared
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Internal.PrettyRef
import           Lens.Micro.Platform
import           Reference

prefix :: Text
prefix = "list: "

throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ prefix <> e

runList :: Args -> CmdInput -> CmdOutput
runList args input = do
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  let cwd     = cwdin input
      refs    = refsin input
      numRefs = fst $ IM.findMax refs
  -- Parse arguments
  refnos <- parseInCommand pRefnos args prefix
  -- TODO: this only works with non-piped input. Need to get it to work properly
  -- with piped input. The problem is we can't reuse getActiveRefnos because
  -- that errors on empty input -- and for this function in particular, empty
  -- input is acceptable. end TODO
  -- Check for any refnos that don't exist
  errorOnInvalidRefnos prefix refnos input
  -- If we reached here, everything is good
  let refnosToPrint =
        if IS.null refnos then IS.fromList [1 .. numRefs] else refnos
      refsToPrint = refs `IM.restrictKeys` refnosToPrint
  liftIO $ TIO.putStrLn =<< prettify cwd (IM.assocs refsToPrint)
  pure $ SCmdOutput refs (Just refnosToPrint)
