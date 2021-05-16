-- |
-- Module    : Abbotsbury.Cite
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This module, and its submodules, provide the functionality for generating
-- citations from 'Work's. All of the functionality here is re-exported by the
-- top-level module ("Abbotsbury").
module Abbotsbury.Cite
  ( -- * Citation generation
    -- $cite-overview
    cite
    -- * Citation styles
  , Abbotsbury.Cite.Internal.Style(..)
  , Abbotsbury.Cite.Styles.Acs.acsStyle
  , Abbotsbury.Cite.Styles.AcsShort.acsShortStyle
  , Abbotsbury.Cite.Styles.Biblatex.bibStyle
    -- * Output formats
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
import           Abbotsbury.Cite.Styles.Acs
import           Abbotsbury.Cite.Styles.AcsShort
import           Abbotsbury.Cite.Styles.Biblatex
import           Abbotsbury.Work
import           Data.Text                      ( Text )

-- $cite-overview
-- Only this one single function is needed for citation generation. It takes two
-- inputs: the /citation style/ and the /output format/. The /style/ dictates
-- what text is output, as well as an abstract representation of its formatting;
-- and the /format/ takes this abstract formatting and turns it into concrete
-- formatting.
--
-- For example, when specifying 'acsStyle' as the @Style@ to use, the year will
-- be output in bold (according to the [ACS Style
-- Guide](https://pubs.acs.org/doi/full/10.1021/acsguide.40303)). The @Style@
-- only specifies that the year is to be bolded: it does not say anything about
-- the way in which this is achieved. Instead, the @Format@ is what controls
-- this. For example, with the Markdown format, the year will be surrounded by
-- @**@; and with the HTML format, the year will be surrounded with the
-- @\<b\>...\</b\>@ tags.
--
-- One curiosity is the BibLaTeX \'style\'. @abbotsbury@ in fact was originally
-- designed to only output BibLaTeX entries, and was later expanded to include
-- rather more functionality. A BibLaTeX entry is not really a \'citation\' per
-- se; so, it is a bit odd that we call BibLaTeX a \'style\', but there is no
-- other way to do it because it is the /style/ which completely controls what
-- text is produced.

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
