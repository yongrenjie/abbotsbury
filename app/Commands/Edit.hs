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

runEdit :: Args -> CmdInput -> CmdOutput
runEdit args input = do
  let cwd  = cwdin input
      refs = refsin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  refnos   <- parseInCommand pRefnos args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to edit
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  let (refnosToEdit, refsToEdit) = unzip refnosAndRefs
  -- Create a temporary file and dump the YAML into it.
  tmpfile <- liftIO $ makeSystemTempFile "abbot_edit.yaml"
  liftIO $ encodeFile tmpfile refsToEdit
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
       (throwError (prefix <> "vim exited with failure"))
  -- Decode the YAML.
  eitherExcRefs      <- liftIO $ decodeFileEither tmpfile
  -- Check for parse errors. The type annotation specifies which instance of
  -- FromJSON to use.
  newRefs :: [Reference] <- mEither
    eitherExcRefs
    (throwError . (prefix <>) . T.pack . prettyPrintParseException)
    pure
  -- Make sure it's still the same number of references...
  when
    (length newRefs /= length refsToEdit)
    (throwError
      (prefix <> "number of references not the same; discarding changes")
    )
  -- Edit the reference list.
  let newRefnosRefs  = zip refnosToEdit newRefs
      refsout        = foldl
        (\rs (index, newRef) -> rs & ix index .~ newRef)
        refs
        newRefnosRefs
  -- Return the edited reference list
  pure $ SCmdOutput refsout Nothing
