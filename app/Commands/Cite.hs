module Commands.Cite (runCite) where


import           Abbotsbury.Cite
import           Commands.Shared
import           Reference
import           Internal.Copy

import           Control.Monad.Except
import           Data.Either                    ( isLeft )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Lens.Micro.Platform
import           System.Process
import           Text.Megaparsec


throwErrorWithPrefix :: Text -> ExceptT Text IO a
throwErrorWithPrefix e = throwError $ "cite: " <> e


data ReplCiteRules = AcsText
                   | AcsMarkdown
                   | AcsRestructured
                   | AcsHtml
                   | AcsWord
                   | Biblatex
                   deriving (Ord, Eq, Show, Enum, Bounded)


runCite :: Args -> CmdInput -> CmdOutput
runCite args input = do
  let refs = refsin input
  -- If no refs present, error immediately
  when (IM.null refs) (throwErrorWithPrefix "no references found")
  case parse pCite "" args of
    Left bundle -> throwError $ T.pack ("cite: " ++ errorBundlePretty bundle)  -- parse error
    Right (refnos, rules) -> do
      -- First, check for any refnos that don't exist
      let badRefnos = refnos IS.\\ IM.keysSet refs
      unless
        (IS.null badRefnos)
        (throwErrorWithPrefix
          ("reference(s) " <> intercalateCommas badRefnos <> " not found")
        )
      -- Figure out which refnos to print
      refsToCite <- case (varin input, IS.null refnos) of
        (Nothing , True ) -> throwErrorWithPrefix "no references selected"
        (Nothing , False) -> pure $ IM.elems (refs `IM.restrictKeys` refnos)
        (Just set, True ) -> pure $ IM.elems (refs `IM.restrictKeys` set)
        (Just set, False) ->
          throwErrorWithPrefix "cannot specify refnos and a pipe simultaneously"
      -- Generate the citation(s)
      let
        (style, format) = getStyleFormat rules
        citations =
          T.intercalate "\n\n" $ map (cite style format . _work) refsToCite
      -- Print them
      liftIO $ TIO.putStrLn citations
      -- Copy them
      liftIO $ case rules of
        AcsWord -> do
          let htmlLines = map (cite style htmlFormat . _work) refsToCite
          copyHtmlLinesAsRtf htmlLines
        _ -> copy citations
      -- Return basically nothing
      pure $ SCmdOutput refs Nothing


pCite :: Parser (IntSet, ReplCiteRules)
pCite = ((,) <$> pRefnos <*> pOneFormatCaseSens abbrevs (Just Biblatex)) <* eof
 where
  abbrevs = M.fromList
    [ ("T", AcsText)
    , ("M", AcsMarkdown)
    , ("R", AcsRestructured)
    , ("H", AcsHtml)
    , ("W", AcsWord)
    , ("b", Biblatex)
    , ("B", Biblatex)
    ]


getStyleFormat :: ReplCiteRules -> (Style, Format)
getStyleFormat AcsText = (acsStyle, textFormat)
getStyleFormat AcsMarkdown = (acsStyle, markdownFormat)
getStyleFormat AcsRestructured = (acsStyle, restructuredFormat)
getStyleFormat AcsHtml = (acsStyle, htmlFormat)
getStyleFormat AcsWord = (acsStyle, textFormat)
getStyleFormat Biblatex = (bibStyle, textFormat)
