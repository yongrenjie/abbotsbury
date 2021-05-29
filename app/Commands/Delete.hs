module Commands.Delete
  ( runDelete
  ) where

import           Abbotsbury
import           Commands.Shared
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Internal.Monad
import           Internal.Path
import           Lens.Micro.Platform
import           Reference
import           System.Directory               ( removeFile )
import           System.IO.Error                ( isDoesNotExistError )

prefix :: Text
prefix = "delete: "

runDelete :: Args -> CmdInput -> CmdOutput
runDelete args input = do
  let refs = refsin input
      cwd  = cwdin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  refnos     <- parseInCommand pRefnos args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to delete
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  let refnosToDelete = IS.fromList $ map fst refnosAndRefs
  -- If we reached here, all is good...
  let refsToKeep   = IM.elems $ refs `IM.withoutKeys` refnosToDelete
      refsToDelete = IM.elems $ refs `IM.restrictKeys` refnosToDelete
      refsout      = IM.fromList $ zip [1 ..] refsToKeep
      filesToDelete =
        [ getPDFPath t cwd ref | t <- [FullText, SI], ref <- refsToDelete ]
  liftIO $ forM_ filesToDelete removeFileIfExists
  -- Return the new list of references, and don't pipe anything through.
  pure $ SCmdOutput refsout Nothing

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f =
  removeFile f
    `catch` (\e -> if isDoesNotExistError e then pure () else throwIO e)
