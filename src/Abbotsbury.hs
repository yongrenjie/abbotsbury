-- | This top-level module essentially re-exports all the useful functionality
-- of the lower-level modules. Although the functions are documented here, it is
-- probably more helpful to look at the individual modules' documentation as
-- they have more text about the general design of @abbotsbury@.
--
-- In particular:
--
--  * "Abbotsbury.Work" describes the @Work@, which is the fundamental data type
--    which @abbotsbury@ uses.
--  * "Abbotsbury.Crossref" describes how to create @Work@s by fetching data
--    from Crossref.
--  * "Abbotsbury.Cite" has information about how to generate citations from
--    @Work@s.

module Abbotsbury
  ( -- * Fetching metadata from Crossref
    -- ** Getting and parsing raw JSON data
    -- $crossref-parse
    Abbotsbury.Crossref.fetchWork,
    Abbotsbury.Crossref.fetchWorks,
    Abbotsbury.Crossref.fetchWorkWithOptions,
    Abbotsbury.Crossref.fetchWorksWithOptions,
    -- ** What could go wrong
    -- $crossref-exceptions
    Abbotsbury.Crossref.Internal.CrossrefException (..),
    Abbotsbury.Crossref.Internal.getDoiFromException,
    -- ** Manually fixing errors in Crossref metadata
    -- $crossref-journalfix
    Abbotsbury.Crossref.defaultJournalFix,
    Abbotsbury.Crossref.emptyJournalFix,
    -- * Citation generation
    -- $cite-overview
    Abbotsbury.Cite.cite,
    -- ** Citation styles
    Abbotsbury.Cite.Style (..),
    Abbotsbury.Cite.acsStyle,
    Abbotsbury.Cite.bibStyle,
    -- ** Output formats
    Abbotsbury.Cite.Format (..),
    Abbotsbury.Cite.textFormat,
    Abbotsbury.Cite.markdownFormat,
    Abbotsbury.Cite.restructuredFormat,
    Abbotsbury.Cite.htmlFormat,
  )
where

import Abbotsbury.Cite
import Abbotsbury.Crossref
import Abbotsbury.Crossref.Internal
import Network.HTTP.Client ()

-- $crossref-parse
-- The following functions provide ways to directly fetch metadata for a given
-- DOI or DOIs. 'fetchWork' and 'fetchWorks' are the ones you are most likely to
-- use: these basically act as versions of 'fetchWorkWithOptions' and
-- 'fetchWorksWithOptions', but with "default settings".

-- $crossref-exceptions
-- If 'fetchWork' and co. fail, then they will return a @Left
-- CrossrefException@. The possible exceptions are as follows. Often, the most
-- important question is which DOI led to an exception: this can be obtained
-- using 'getDoiFromException'. (You can also pattern-match on the
-- 'CrossrefException' value, but that's a bit verbose.)

-- $crossref-journalfix
-- Journal names should always be abbreviated according to CASSI
-- (<https://cassi.cas.org>). However, Crossref metadata often contains
-- abbreviated journal names which are not in accordance with the CASSI
-- abbreviations. Thus, 'fetchWorkWithOptions' and 'fetchWorksWithOptions'
-- take a @Map Text Text@ as a parameter: this is a @Map@ where the keys are the
-- short journal names that Crossref gives you, and the values are the /correct/
-- short journal names. For example, if Crossref gives the incorrect
-- abbreviation
-- > Nat Rev Chem
-- and you really want it to be (this is based on a real example)
-- > Nat. Rev. Chem.
-- then you should pass @M.fromList [("Nat Rev Chem", "Nat. Rev. Chem.")]@ as
-- the argument to 'fetchWorkWithOptions'.
--
-- @abbotsbury@ itself already has collected a bunch of common mistakes (mainly
-- for chemistry-focused journals) which the author or others have noticed.
-- These mistakes, and the fixes for them, are collected in 'defaultJournalFix'.
-- Thus, if you have fixes that you want to add, you can use the functions from
-- "Data.Map" (such as 'Data.Map.union') to add them to 'defaultJournalFix'.
-- Alternatively, [submit a pull request
-- :-)](https://github.com/yongrenjie/abbotsbury/)

-- $cite-overview
-- Only this one single function is needed for citation generation. The
-- /citation style/ dictates what text is output, and the /output format/
-- dictates how this text is formatted. For example, the ACS style specifies
-- that the year must be in bold. The exact way in which this is achieved
-- depends on the output format: for example, with the Markdown format, the year
-- is surrounded by @**@; and with the HTML format, the year is surrounded with
-- the @\<b\>...\</b\>@ tags.
--
-- One curiosity is the BibLaTeX \'style\'. @abbotsbury@ in fact was originally
-- designed to only output BibLaTeX entries, and was later expanded to include
-- rather more functionality. A BibLaTeX entry is not really a \'citation\' per
-- se; so, it is a bit odd that we call BibLaTeX a \'style\', but there is no
-- other way to do it because it is the /style/ which completely controls what
-- text is produced.
