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
  ( cite
  , Abbotsbury.Cite.Internal.Style(..)
  , Abbotsbury.Cite.Styles.ACS.acsStyle
  , Abbotsbury.Cite.Styles.Biblatex.bibStyle
  , Abbotsbury.Cite.Internal.Format(..)
  , Abbotsbury.Cite.Formats.Text.textFormat
  , Abbotsbury.Cite.Formats.Markdown.markdownFormat
  , Abbotsbury.Cite.Formats.Restructured.restructuredFormat
  , Abbotsbury.Cite.Formats.HTML.htmlFormat
  ) where

import           Abbotsbury.Cite.Formats.HTML
import           Abbotsbury.Cite.Formats.Markdown
import           Abbotsbury.Cite.Formats.Restructured
import           Abbotsbury.Cite.Formats.Text
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Cite.Styles.ACS
import           Abbotsbury.Cite.Styles.Biblatex
import           Abbotsbury.Work
import           Data.Text                      ( Text )


-- | Generate a citation for a work.
cite
  ::
  -- | Citation style to use.
     Style
  ->
  -- | Output formatting to use.
     Format
  ->
  -- | The work to cite.
     Work
  ->
  -- | The citation.
     Text
cite style format = formatCitationPart format . makeCitationPart style
