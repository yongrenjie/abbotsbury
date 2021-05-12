module Commands.Delete
  ( runDelete
  ) where

import           Abbotsbury
import           Commands.Shared
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Internal.Path
import           Lens.Micro.Platform
import           Reference
import           System.Directory               ( removeFile )
import           System.IO.Error                ( isDoesNotExistError )
import           Text.Megaparsec                ( eof
                                                , parse
                                                )

prefix :: Text
prefix = "delete: "

throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ "delete: " <> e

runDelete :: Args -> CmdInput -> CmdOutput
runDelete args input = do
  let refs = refsin input
      cwd  = cwdin input
  -- If no refs present, error immediately
  when (IM.null refs) (throwErrorWithPrefix "no references found")
  -- Parse arguments
  refnos <- parseInCommand (pRefnos <* eof) args prefix
  -- First, check for any refnos that don't exist
  let badRefnos = refnos IS.\\ IM.keysSet refs
  unless
    (IS.null badRefnos)
    (throwErrorWithPrefix
      ("reference(s) " <> intercalateCommas badRefnos <> " not found")
    )
  -- If we reached here, all is good...
  let remainingRefs = IM.elems (refs `IM.withoutKeys` refnos)
      refsout       = IM.fromList $ zip [1 ..] remainingRefs
  -- Also make sure to delete all the PDFs
  let refsToBeDeleted = IM.elems (refs `IM.restrictKeys` refnos)
      filesToDelete =
        [ getPDFPath t cwd ref | t <- [FullText, SI], ref <- refsToBeDeleted ]
  liftIO $ forM_ filesToDelete removeFileIfExists
  -- Return the new list of references.
  pure $ SCmdOutput refsout Nothing
 where
  removeFileIfExists :: FilePath -> IO ()
  removeFileIfExists f =
    removeFile f
      `catch` (\e -> if isDoesNotExistError e then pure () else throwIO e)
