module Commands.Addpdf
  ( runAddpdf
  ) where

import           Abbotsbury.Work
import           Commands.Shared
import           Control.Exception              ( IOException(..)
                                                , catch
                                                )
import           Data.Char                      ( isSpace )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.MInputT
import           Internal.Monad
import           Internal.Path
import           Internal.Style                 ( setBold )
import           Lens.Micro.Platform
import           Replace.Megaparsec             ( streamEdit )
import           System.Directory               ( doesFileExist )
import           Text.Megaparsec
import           Text.Megaparsec.Char

prefix :: Text
prefix = "addpdf: "

runAddpdf :: Args -> CmdInput -> CmdOutput
runAddpdf args input = do
  let cwd  = cwdin input
      refs = refsin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  refnos <- parseInCommand pRefnos args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to add PDFs for
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  -- For each refno...
  liftIO $ forM_ refnosAndRefs $ \(rno, ref) -> do
    TIO.putStrLn . setBold $ refnoT rno <> getTitle ref
    let pdfTypes = getValidPdfTypes ref
    forM_ pdfTypes $ \t -> do
      -- Bring in haskeline here, which nicely automatically gives us the
      -- correct Ctrl-D and Ctrl-C behaviour, as well as readline bindings.
      let dest = getPdfPath t cwd ref
      destExists <- doesFileExist dest
      if destExists
        then TIO.putStrLn $ showPdfType t <> " already exists; skipping"
        else do
          maybeSrc <- mRunInputT
            defaultSettings
            (mGetInputLine $ "enter path to " <> showPdfType t <> ": ")
          case maybeSrc of
            Just s
              | T.null s -> pure ()
              | T.head s == '/' -> do   -- file path
                let src = T.unpack . processCmdlineInput $ s
                copyWithMkdir src dest
              | otherwise -> do         -- not a file path, assume it's a URL
                eitherEmail <- runExceptT (getUserEmail prefix)
                case eitherEmail of
                  Left  err   -> printError err
                  Right email -> downloadPdf email Nothing s dest >> pure ()
            Nothing -> pure ()  -- silently fail on Ctrl-D
  pure $ SCmdOutput refs Nothing

-- | This is a very crude way of removing escaped characters: basically, a
-- backslash followed by any character X is just replaced with X itself.
processCmdlineInput :: Text -> Text
processCmdlineInput = streamEdit pBackslashX T.singleton . stripExtraSpace
 where
  pBackslashX :: Parser Char
  pBackslashX = char '\\' *> anySingle
  stripExtraSpace :: Text -> Text
  stripExtraSpace t = T.stripStart t'
   where
    trailingSpace = T.takeWhileEnd isSpace t
    remaining     = T.stripEnd t
    t' | T.null trailingSpace     = t
       | T.last remaining == '\\' = remaining <> T.take 1 trailingSpace
       | otherwise                = remaining
