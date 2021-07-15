module Internal.Copy where

import           Control.Exception              ( SomeException
                                                , handle
                                                )
import           Data.Char                      ( ord )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.IO                      ( stderr )
import           System.Info                    ( os )
import           System.Process                 ( CreateProcess(std_in)
                                                , ProcessHandle
                                                , StdStream(CreatePipe)
                                                , createProcess
                                                , shell
                                                )

-- | Copies text to the clipboard, silently ignoring ALL exceptions.
copy :: Text -> IO (Maybe ProcessHandle)
copy t =
  handle
      (\(e :: SomeException) -> do
        TIO.hPutStrLn stderr "abbot: copy to clipboard failed"
        pure Nothing
      )
    $ case os of
        "darwin"  -> t `pipedInto` "pbcopy"
        "mingw32" -> t `pipedInto` "clip"
        _         -> do
          TIO.hPutStrLn
            stderr
            ("abbot: copy not supported on operating system " <> T.pack os)
          pure Nothing

-- | Converts a piece of HTML into rich text and then copies it to the clipboard. This is useful for
-- getting preformatted text in Word.
-- Note that this only works on macOS.
-- https://www.ostricher.com/2015/08/from-markdown-to-pastable-formatted-text-in-os-x-terminal/
copyHtmlAsRtf :: Text -> IO (Maybe ProcessHandle)
copyHtmlAsRtf htmlText =
  handle
      (\(e :: SomeException) -> do
        TIO.hPutStrLn stderr "abbot: RTF copy to clipboard failed"
        pure Nothing
      )
    $ case os of
        "darwin" ->
          htmlText
            `pipedInto` "textutil -convert rtf -stdin -stdout -inputencoding UTF-8 -format html | pbcopy -Prefer rtf"
        "mingw32" ->
          escapedHtmlText
            `pipedInto` "powershell -Command \"$input | Set-Clipboard -AsHtml\""
         where
          -- see https://stackoverflow.com/questions/47474346
          escapedHtmlText = T.concatMap escape htmlText
          escape c | ord c > 127 = "&#" <> (T.pack . show $ ord c) <> ";"
                   | otherwise   = T.singleton c
        _ -> do
          TIO.hPutStrLn
            stderr
            ("abbot: RTF copy not supported on operating system " <> T.pack os)
          pure Nothing

-- | Helper function which surrounds each line with <p>...</p> and runs copyHtmlAsRtf on the whole
-- thing.
copyHtmlLinesAsRtf :: [Text] -> IO (Maybe ProcessHandle)
copyHtmlLinesAsRtf =
  copyHtmlAsRtf . T.concat . map (\t -> "<p>" <> t <> "</p>")

-- | @stdin `pipedInto` command@ pipes the Text @stdin@ into the specified
-- command.
pipedInto :: Text -> String -> IO (Maybe ProcessHandle)
pipedInto stdin cmd = do
  (Just hIn, _, _, ph) <- createProcess (shell cmd) { std_in = CreatePipe }
  TIO.hPutStr hIn stdin
  pure $ Just ph
