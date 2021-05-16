module Commands.New
  ( runNew
  ) where

import           Abbotsbury
import           Commands.Shared
import qualified Data.ByteString               as BS
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Yaml                      ( ParseException
                                                , decodeFileEither
                                                , encodeFile
                                                , prettyPrintParseException
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
                                                , SeekMode(AbsoluteSeek)
                                                , hFlush
                                                , hSeek
                                                , withFile
                                                )
import           System.IO.Temp                 ( withSystemTempFile )
import           System.Process                 ( CreateProcess(std_in, std_out)
                                                , StdStream(UseHandle)
                                                , createProcess
                                                , proc
                                                , waitForProcess
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char

prefix :: Text
prefix = "new: "

runNew :: Args -> CmdInput -> CmdOutput
runNew args input = do
  let cwd  = cwdin input
      refs = refsin input
  -- Parse arguments
  emptyWork <- parseInCommand pWorkType args prefix
  -- Create the empty reference
  now <- liftIO getCurrentTime
  let emptyRef = Reference emptyWork S.empty now now
  -- Create a temporary file and dump the YAML into it.
  tmpfile <- liftIO $ makeSystemTempFile "abbot_new.yaml"
  liftIO $ encodeFile tmpfile emptyRef
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
  newRef :: Reference <- mEither
    eitherExcRefs
    (throwError . (prefix <>) . T.pack . prettyPrintParseException)
    pure
  -- Edit the reference list.
  let newRefno = IM.size refs + 1
      refsout  = IM.insert newRefno newRef refs
  -- Return the newed reference list
  pure $ SCmdOutput refsout Nothing

pWorkType :: Parser Work
pWorkType = choice
  -- To make the YAML easier to edit, we create a phantom author.
  [ ArticleWork emptyArticle <$ string' "article"
  , BookWork (emptyBook & authors .~ [emptyPerson]) <$ string' "book"
  ]
 where
  emptyPerson :: Person
  emptyPerson = Person (Just "") ""
