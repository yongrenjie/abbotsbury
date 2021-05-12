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
  when (IM.null $ refsin input) (throwErrorWithPrefix "no references found")
  let cwd     = cwdin input
      refs    = refsin input
      numRefs = fst $ IM.findMax refs
  -- Parse arguments
  refnos <- parseInCommand pRefnos args prefix
  -- First, check for any refnos that don't exist
  let badRefnos = refnos IS.\\ IM.keysSet refs
  unless
    (IS.null badRefnos)
    (throwErrorWithPrefix
      ("reference(s) " <> intercalateCommas badRefnos <> " not found")
    )
  -- If we reached here, everything is good
  let refnosToPrint =
        if IS.null refnos then IS.fromList [1 .. numRefs] else refnos
      refsToPrint = refs `IM.restrictKeys` refnosToPrint
  liftIO $ TIO.putStrLn =<< prettify cwd (IM.assocs refsToPrint)
  pure $ SCmdOutput refs (Just refnosToPrint)
