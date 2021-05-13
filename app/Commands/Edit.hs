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
                                                , decodeEither'
                                                , encode
                                                )
import           Internal.Monad
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
  eitherExitcodeBs <- liftIO $ withSystemTempFile "abbot_edit.yaml" $ \fp hdl ->
    do
      -- Dump the YAML to a tempfile.
      BS.hPut hdl (encode worksToEdit)
      hFlush hdl  -- this is necessary otherwise the file will be empty in vim!
      -- Then open the tempfile in vim, and edit it. The /dev/tty handles are
      -- required to stop vim from complaining (its stdin and stdout must be
      -- from and to a terminal).
      withFile "/dev/tty" ReadMode $ \inH -> do
        withFile "/dev/tty" WriteMode $ \outH -> do
          (_, _, _, ph) <- createProcess (proc "vim" [fp])
            { std_in  = UseHandle inH
            , std_out = UseHandle outH
            }
          exitCode <- waitForProcess ph
          -- Then read the results of that if vim exited successfully.
          -- BS.hGetContents hdl seems to give us the *old* data... (you also
          -- need to `hSeek hdl AbsoluteSeek 0` before that). Not sure why.
          -- Operating system detail, I guess.
          case exitCode of
            ExitSuccess -> pure <$> BS.readFile fp
            ExitFailure i ->
              pure $ Left ("vim exited with code " <> T.pack (show i))
  -- A nice helper function.
  let mEither :: Monad m => Either a b -> (a -> m c) -> (b -> m c) -> m c
      mEither (Left  a) f1 _  = f1 a
      mEither (Right b) _  f2 = f2 b
  -- Decode the YAML.
  (eitherErrorWorks :: Either ParseException [Work]) <- mEither
    eitherExitcodeBs
    throwErrorWithPrefix
    (pure . decodeEither')
  -- Check for parse errors.
  newWorks <- mEither
    eitherErrorWorks
    (throwErrorWithPrefix . T.pack . prettyPrintParseException)
    pure
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
