module Internal.Copy where


import qualified Control.Exception             as CE
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Process
import           Control.Monad
import           System.Info
import           System.IO


-- | Copies text to the clipboard, silently ignoring ALL exceptions.
copy :: Text -> IO ()
copy t =
  CE.handle
      (\(e :: CE.SomeException) ->
        TIO.hPutStrLn stderr "abbot: copy to clipboard failed"
      )
    $ case os of
        "darwin" -> do
          (Just hIn, _, _, _) <- createProcess (proc "pbcopy" [])
            { std_in = CreatePipe
            }
          TIO.hPutStr hIn t
        _ -> TIO.hPutStrLn
          stderr
          ("abbot: copy not supported on operating system " <> T.pack os)
