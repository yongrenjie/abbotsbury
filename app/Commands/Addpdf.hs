module Commands.Addpdf
  ( runAddpdf
  ) where

import           Abbotsbury.Work
import           Commands.Shared
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Internal.Path
import           Internal.Style                 ( setBold )
import           Lens.Micro.Platform
import           System.IO                      ( hFlush
                                                , stdout
                                                )

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
    TIO.putStr "enter path to PDF: "
    hFlush stdout
    fpath <- TIO.getLine
    TIO.putStrLn $ "getting from " <> fpath <> "..."
    printError "not yet implemented"
  throwError "uh oh"
