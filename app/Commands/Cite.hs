module Commands.Cite
  ( runCite
  ) where

import           Abbotsbury.Cite
import           Commands.Shared
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
import           Internal.Copy
import           Internal.Monad
import           Lens.Micro.Platform
import           Reference
import           System.Process

prefix :: Text
prefix = "cite: "

data ReplCiteRules
  = AcsText
  | AcsMarkdown
  | AcsRestructured
  | AcsHtml
  | AcsWord
  | AcsShortText
  | AcsShortMarkdown
  | AcsShortRestructured
  | AcsShortHtml
  | AcsShortWord
  | Biblatex
  deriving (Ord, Eq, Show, Enum, Bounded)

runCite :: Args -> CmdInput -> CmdOutput
runCite args input = do
  let refs = refsin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  (refnos, rules) <- parseInCommand pCite args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to cite
  refnosAndRefs <- getActiveRefs prefix argsRefnos True input
  -- Generate the citation(s)
  let refsToCite = map snd refnosAndRefs
  let (style, format) = getStyleFormat rules
      citations =
        T.intercalate "\n\n" $ map (cite style format . _work) refsToCite
  -- Print them
  liftIO $ TIO.putStrLn citations
  -- Copy them to the clipboard
  liftIO $ if rules `elem` [AcsWord, AcsShortWord]
    then do
      let htmlLines = map (cite style htmlFormat . _work) refsToCite
      case htmlLines of
           [c] -> copyHtmlAsRtf c
           _   -> copyHtmlLinesAsRtf htmlLines
    else copy citations
  -- Return basically nothing
  pure $ SCmdOutput (refsin input) Nothing

pCite :: Parser (Refnos, ReplCiteRules)
pCite = (,) <$> pRefnos <*> pOneFormatCaseSens abbrevs (Just Biblatex)
 where
  abbrevs = M.fromList
    [ ("T", AcsText)
    , ("M", AcsMarkdown)
    , ("R", AcsRestructured)
    , ("H", AcsHtml)
    , ("W", AcsWord)
    , ("t", AcsShortText)
    , ("m", AcsShortMarkdown)
    , ("r", AcsShortRestructured)
    , ("h", AcsShortHtml)
    , ("w", AcsShortWord)
    , ("b", Biblatex)
    , ("B", Biblatex)
    ]

getStyleFormat :: ReplCiteRules -> (Style, Format)
getStyleFormat AcsText              = (acsStyle, textFormat)
getStyleFormat AcsMarkdown          = (acsStyle, markdownFormat)
getStyleFormat AcsRestructured      = (acsStyle, restructuredFormat)
getStyleFormat AcsHtml              = (acsStyle, htmlFormat)
getStyleFormat AcsWord              = (acsStyle, textFormat)
getStyleFormat AcsShortText         = (acsShortStyle, textFormat)
getStyleFormat AcsShortMarkdown     = (acsShortStyle, markdownFormat)
getStyleFormat AcsShortRestructured = (acsShortStyle, restructuredFormat)
getStyleFormat AcsShortHtml         = (acsShortStyle, htmlFormat)
getStyleFormat AcsShortWord         = (acsShortStyle, textFormat)
getStyleFormat Biblatex             = (bibStyle, textFormat)
