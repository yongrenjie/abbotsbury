-- |
-- Module    : Abbotsbury
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This top-level module (re-)exports all of the functionality that you, as an
-- end user, will likely use. Therefore, full explanatory documentation is
-- provided here, and not in the lower-level modules.
--
-- However, it is worth adding a note about the organisation of the project. The
-- main submodules are:
--
--  * "Abbotsbury.Work" defines the @Work@, which is the fundamental data type
--    used by @abbotsbury@.
--  * "Abbotsbury.Crossref" provides functions for creating @Work@s by fetching
--    data from Crossref.
--  * "Abbotsbury.Cite" generates citations from @Work@s.

module Abbotsbury
  ( -- * Fetching metadata from Crossref
    -- ** Getting and parsing raw JSON data
    -- $crossref-parse
    Abbotsbury.Crossref.fetchWork
  , Abbotsbury.Crossref.fetchWorks
  , Abbotsbury.Crossref.fetchWork'
  , Abbotsbury.Crossref.fetchWorks'
  ,
    -- ** What could go wrong
    -- $crossref-exceptions
    Abbotsbury.Crossref.Internal.CrossrefException(..)
  , Abbotsbury.Crossref.Internal.getDoiFromException
  ,
    -- ** Manually fixing errors in Crossref metadata
    -- $crossref-journalfix
    Abbotsbury.Crossref.defaultJournalFix
  , Abbotsbury.Crossref.emptyJournalFix
  ,
    -- * Citation generation
    -- $cite-overview
    Abbotsbury.Cite.cite
  ,
    -- ** Citation styles
    Abbotsbury.Cite.Style(..)
  , Abbotsbury.Cite.acsStyle
  , Abbotsbury.Cite.bibStyle
  ,
    -- ** Output formats
    Abbotsbury.Cite.Format(..)
  , Abbotsbury.Cite.textFormat
  , Abbotsbury.Cite.markdownFormat
  , Abbotsbury.Cite.restructuredFormat
  , Abbotsbury.Cite.htmlFormat
  ) where

import           Abbotsbury.Cite
import           Abbotsbury.Crossref
import           Abbotsbury.Crossref.Internal
import           Network.HTTP.Client            ( )

-- $crossref-parse
-- The following functions provide ways to directly fetch metadata for a given
-- DOI or DOIs. 'fetchWork' and 'fetchWorks' are the ones you are most likely to
-- use: these basically act as versions of 'fetchWork'' and 'fetchWorks'', but
-- with "default settings".

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
-- abbreviations. Thus, 'fetchWork'' and 'fetchWorks'' take a @Map Text Text@ as
-- a parameter: this is a @Map@ where the keys are the long (i.e. unabbreviated)
-- journal names that Crossref gives you, and the values are the /correct/ short
-- journal names. For example, if you want to specify that
-- 
-- > Nature Reviews Chemistry
--
-- should always be abbreviated as
--
-- > Nat. Rev. Chem.
--
-- then you should pass @M.fromList [("Nature Reviews Chemistry", "Nat. Rev.
-- Chem.")]@ as the argument to 'fetchWork''.
--
-- @abbotsbury@ itself already has collected a bunch of common mistakes (mainly
-- for chemistry-focused journals) which the author or others have noticed.
-- These mistakes, and the fixes for them, are collected in 'defaultJournalFix'.
-- Thus, if you have fixes that you want to add, you can use the functions from
-- "Data.Map" (such as 'Data.Map.union') to add them to 'defaultJournalFix'.
-- Alternatively, [submit a pull request
-- :-)](https://github.com/yongrenjie/abbotsbury/)

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
