module Abbot.Commands.Cite 
  ( module Abbot.Commands.Cite
    ) where


import           Abbot.Commands.Shared
import           Abbot.Reference
-- import           Abbot.Style                    ( makeError )

import qualified Control.Exception             as CE
import           Data.Either                    ( isLeft )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( liftIO
                                                , throwError
                                                )
-- import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
-- import           Data.Map                       ( Map )
import qualified Data.Map                      as M
-- import           Data.Set                       ( Set )
-- import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Text.Megaparsec
import           System.Process


-- For now
data CiteFormat = PlainShort
                | PlainLong
                deriving (Ord, Eq, Show, Enum, Bounded)


runCite :: Args -> CmdInput -> CmdOutput
runCite args input = do
  let refs = refsin input
  -- If no refs present, error immediately
  when (IM.null refs) (throwError "cite: no references found")
  case parse pCite "" args of
    Left bundle -> throwError $ T.pack ("cite: " ++ errorBundlePretty bundle)  -- parse error
    Right (refnos, format) -> do
      -- First, check for any refnos that don't exist
      let badRefnos = refnos IS.\\ IM.keysSet refs
      unless
        (IS.null badRefnos)
        (throwError
          ("cite: reference(s) " <> intercalateCommas badRefnos <> " not found")
        )
      -- Then, check if refnos is empty
      when (IS.null refnos) (throwError "open: no references selected")
      -- Generate the citation(s)
      let citations = T.unlines $ fmap (makeCitation format . (refs IM.!)) (IS.toList refnos)
      -- Print them
      liftIO $ TIO.putStr citations
      -- Copy them to clipboard (assuming macOS)
      -- The exception handling is not exactly elegant
      let copy = do (Just hIn, _, _, _) <- liftIO $ createProcess (proc "pbcopy" []) { std_in = CreatePipe }
                    liftIO $ TIO.hPutStr hIn citations
      copyResult <- liftIO (CE.try copy :: IO (Either CE.SomeException ()))
      when (isLeft copyResult) (printError "copy failed")
      -- Return basically nothing
      pure $ SCmdOutput refs Nothing


makeCitation :: CiteFormat -> Reference -> Text
makeCitation fmt ref =
  case fmt of
       PlainShort -> "Hello" <> (_title . _work $ ref)
       PlainLong -> "There" <> (_title . _work $ ref)


pCite :: Parser (IntSet, CiteFormat)
pCite = ((,) <$> pRefnos <*> pOneFormatCaseSens abbrevs (Just PlainShort)) <* eof
 where
  abbrevs = M.fromList
    [ ("p", PlainShort)
    , ("P", PlainLong)
    ]
