module Commands.Edit
  ( runEdit
  ) where

import           Commands.Shared
import qualified Data.ByteString               as BS
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Yaml                      ( ParseException
                                                , prettyPrintParseException
                                                , decodeFileEither
                                                , encodeFile
                                                )
import           Internal.Monad
import           Internal.Path                  ( makeSystemTempFile )
import           Lens.Micro.Platform
import           Reference
import           System.Exit                    ( ExitCode
                                                  ( ExitFailure
                                                  , ExitSuccess
                                                  )
                                                )
import           System.IO                      ( IOMode(ReadMode, WriteMode)
                                                , hFlush
                                                , hSeek
                                                , SeekMode(AbsoluteSeek)
                                                , withFile
                                                )
import           System.IO.Temp                 ( withSystemTempFile )
import           System.Process                 ( CreateProcess(std_in, std_out)
                                                , StdStream(UseHandle)
                                                , createProcess
                                                , proc
                                                , waitForProcess
                                                )

prefix :: Text
prefix = "edit: "

throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ prefix <> e

runEdit :: Args -> CmdInput -> CmdOutput
runEdit args input = do
  -- If no refs present, error immediately
  when (IM.null $ refsin input) (throwErrorWithPrefix "no references found")
  let cwd  = cwdin input
      refs = refsin input
  -- Parse arguments
  refnos       <- parseInCommand pRefnos args prefix
  -- Figure out which refnos to edit
  refnosToEdit <- case (varin input, IS.null refnos) of
    (Nothing , True ) -> throwErrorWithPrefix "no references selected"
    (Nothing , False) -> pure refnos
    (Just set, True ) -> pure set
    (Just set, False) ->
      throwErrorWithPrefix "cannot specify refnos and a pipe simultaneously"
  -- Check for any refnos that don't exist
  let badRefnos = refnosToEdit IS.\\ IM.keysSet refs
  unless
    (IS.null badRefnos)
    (throwErrorWithPrefix
      ("reference(s) " <> intercalateCommas badRefnos <> " not found")
    )
  -- Get the Works.
  let refsToEdit  = IM.elems $ refs `IM.restrictKeys` refnosToEdit
      worksToEdit = map (^. work) refsToEdit
  -- Create a temporary file and dump the YAML into it.
  tmpfile <- liftIO $ makeSystemTempFile "abbot_edit.yaml"
  liftIO $ encodeFile tmpfile worksToEdit
  -- Then open the tempfile in vim, and edit it. The /dev/tty handles are
  -- required to stop vim from complaining (its stdin and stdout must be from
  -- and to a terminal).
  exitCode <- liftIO $ withFile "/dev/tty" ReadMode $ \inH -> do
    withFile "/dev/tty" WriteMode $ \outH -> do
      (_, _, _, ph) <- createProcess (proc "vim" [tmpfile])
        { std_in  = UseHandle inH
        , std_out = UseHandle outH
        }
      waitForProcess ph
  when (exitCode /= ExitSuccess)
       (throwErrorWithPrefix "vim exited with failure")
  -- Decode the YAML.
  eitherExcWorks     <- liftIO $ decodeFileEither tmpfile
  -- Check for parse errors. The type annotation specifies which instance of
  -- FromJSON to use.
  newWorks :: [Work] <- case eitherExcWorks of
    Left  e  -> throwErrorWithPrefix . T.pack . prettyPrintParseException $ e
    Right ws -> pure ws
  -- Make sure it's still the same number of references...
  when
    (length newWorks /= length worksToEdit)
    (throwErrorWithPrefix
      "number of references not the same; discarding changes"
    )
  -- Edit the reference list.
  let newRefnosWorks = zip (IS.toList refnosToEdit) newWorks
      refsout        = foldl
        (\rs (index, newWork) -> rs & (ix index . work) .~ newWork)
        refs
        newRefnosWorks
  -- Return the edited reference list
  pure $ SCmdOutput refsout Nothing
