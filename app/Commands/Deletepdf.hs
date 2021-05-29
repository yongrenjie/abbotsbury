module Commands.Deletepdf
  ( runDeletepdf
  ) where

import           Commands.Shared
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Internal.Path

prefix :: Text
prefix = "deletepdf: "

runDeletepdf :: Args -> CmdInput -> CmdOutput
runDeletepdf args input = do
  let cwd  = cwdin input
      refs = refsin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  (refnos, types) <- parseInCommand pDeletePdf args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to delete PDFs for
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  let refsToDeletePdf = map snd refnosAndRefs
  -- Construct jobs
  let filesToDelete =
        [ getPdfPath pdft cwd ref
        | ref  <- refsToDeletePdf
        , pdft <- S.toList types
        , pdft `elem` getValidPdfTypes ref
        ]
  liftIO $ forM_ filesToDelete removeFileIfExists
  -- Return the new list of references, and don't pipe anything through.
  pure $ SCmdOutput refs Nothing

pDeletePdf :: Parser (Refnos, Set PdfType)
pDeletePdf = (,) <$> pRefnos <*> pFormats abbrevs (Just FullText)
 where
  abbrevs = M.fromList
    [ ("p"  , FullText)
    , ("pdf", FullText)
    , ("s"  , SI)
    , ("si" , SI)
    ]
