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

runList :: Args -> CmdInput -> CmdOutput
runList args input = do
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  let cwd     = cwdin input
      refs    = refsin input
      numRefs = fst $ IM.findMax refs
  -- Parse arguments
  refnos' <- parseInCommand pRefnos args prefix
  let argsRefnos = resolveRefnosWith refs refnos'
  refnosAndRefs <- getActiveRefs prefix argsRefnos False input
  let refnos = IS.fromList $ map fst refnosAndRefs
  -- If we reached here, everything is good
  let refnosToPrint =
        if IS.null refnos then IS.fromList [1 .. numRefs] else refnos
      refsToPrint = refs `IM.restrictKeys` refnosToPrint
  liftIO $ TIO.putStrLn =<< prettify cwd (IM.assocs refsToPrint)
  pure $ SCmdOutput refs (Just refnosToPrint)
