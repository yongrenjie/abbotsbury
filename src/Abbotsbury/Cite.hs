-- |
-- Module    : Abbotsbury.Cite
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This module, and its submodules, provide the functionality for generating
-- citations from 'Work's. These modules mainly contain functions which are
-- exposed for testing purposes. If you are looking for usage guidance, please
-- go to the top-level module "Abbotsbury".
module Abbotsbury.Cite
  ( cite,
    makeParts,
    formatParts,
    formatOnePart,
    Abbotsbury.Cite.Internal.Style (..),
    Abbotsbury.Cite.Styles.ACS.acsStyle,
    Abbotsbury.Cite.Styles.Biblatex.bibStyle,
    Abbotsbury.Cite.Internal.Format (..),
    Abbotsbury.Cite.Formats.Text.textFormat,
    Abbotsbury.Cite.Formats.Markdown.markdownFormat,
    Abbotsbury.Cite.Formats.Restructured.restructuredFormat,
    Abbotsbury.Cite.Formats.HTML.htmlFormat,
  )
where

import Abbotsbury.Cite.Formats.HTML
import Abbotsbury.Cite.Formats.Markdown
import Abbotsbury.Cite.Formats.Restructured
import Abbotsbury.Cite.Formats.Text
import Abbotsbury.Cite.Internal
import Abbotsbury.Cite.Styles.ACS
import Abbotsbury.Cite.Styles.Biblatex
import Abbotsbury.Work
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro

-- | Generate a citation for a work.
cite ::
  -- | Citation style to use.
  Style ->
  -- | Output formatting to use.
  Format ->
  -- | The work to cite.
  Work ->
  -- | The citation.
  Text
cite style format = formatParts format . makeParts style

-- | Using a citation style, generate a list of CitationParts (i.e. Abbotsbury's internal abstract
-- representation of formatted text).
makeParts :: Style -> Work -> [CitationPart]
makeParts style work = case wt of
  JournalArticle -> articleConstructor style work
  _ -> [CText ("work type " <> tshow wt <> " not supported yet")]
  where
    wt = work ^. workType
    tshow = T.pack . show

-- | Using a citation format, generate text that has concrete formatting from the abstractly
-- formatted CitationParts.
formatParts :: Format -> [CitationPart] -> Text
formatParts format = T.concat . map (formatOnePart format)

-- | Using a citation format, generate text that has concrete formatting from one single
-- CitationPart.
formatOnePart ::
  -- | The citation output format to be used.
  Format ->
  -- | The citation part to format.
  CitationPart ->
  Text
formatOnePart fmt@(Format plainFormatter boldFormatter italicFormatter linkFormatter) part =
  case part of
    (CText t) -> plainFormatter t
    (Bold part') -> boldFormatter (formatOnePart fmt part')
    (Italic part') -> italicFormatter (formatOnePart fmt part')
    (Link uri part') -> linkFormatter uri (formatOnePart fmt part')
