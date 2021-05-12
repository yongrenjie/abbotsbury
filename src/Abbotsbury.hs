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
  ( -- * The core datatypes
    module Abbotsbury.Work
    -- * Fetching metadata from Crossref
    -- ** Getting and parsing raw JSON data
    -- $crossref-parse
    -- $crossref-journalfix
  , Abbotsbury.Crossref.fetchWork
  , Abbotsbury.Crossref.fetchWorks
  , Abbotsbury.Crossref.fetchWork'
  , Abbotsbury.Crossref.fetchWorks'
  ,
    -- ** What could go wrong
    -- $crossref-exceptions
    Abbotsbury.Crossref.Internal.CrossrefException(..)
  , Abbotsbury.Crossref.Internal.getDoiFromException
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
import           Abbotsbury.Work
import           Network.HTTP.Client            ( )

-- $crossref-parse
-- The following functions provide ways to directly fetch metadata for a given
-- DOI or DOIs. 'fetchWork' and 'fetchWorks' are the ones you are most likely to
-- use: these basically act as versions of 'fetchWork'' and 'fetchWorks'', but
-- with "default settings".

-- $crossref-journalfix
-- Journal names should always be abbreviated according to CASSI
-- (<https://cassi.cas.org>). However, Crossref metadata often contains
-- abbreviated journal names which are not in accordance with the CASSI
-- abbreviations. (Actually, this is not Crossref's fault; it is the fault of
-- the publisher who deposits metadata with Crossref, but to an end user like
-- us, this distinction is immaterial.)
--
-- To deal with this, 'fetchWork'' and 'fetchWorks'' take a @Bool@ as a
-- parameter. If this is @True@, then @abbotsbury@ will use an internally
-- compiled @Map@ of abbreviated journal names to find the CASSI-compliant
-- abbreviations. If the internal @Map@ does not contain the requested journal,
-- then it will fall back on Crossref's data. If it is instead set to @False@,
-- then @abbotsbury@ will just use the Crossref data directly.
-- 
-- The basic (no-apostrophe) functions 'fetchWork' and 'fetchWorks' use the
-- internal @Map@ by default. If you obtain incorrect metadata in the @Work@
-- (i.e. the journal doesn't appear in @abbotsbury@'s internal @Map@), then
-- then you can manually replace the @journalShort@ field, as demonstrated below
-- (with lenses, although you can do this with plain old record update syntax).
-- Alternatively, [submit a pull request
-- :-)](https://github.com/yongrenjie/abbotsbury/)
-- 
-- Note that this following example uses 'fetchWork'' because @abbotsbury@'s
-- internal @Map@ of abbreviations already contains the corrected form for /Nature/
-- /Communications/.
--
-- >>> import Lens.Micro         -- for (&), (^.), and (.~)
-- >>> Right natComm <- fetchWork' Nothing False "your@email.com" "10.1038/s41467-021-21936-4"
-- >>> natComm
-- Work {_workType = JournalArticle, _title = "Direct catalytic asymmetric synthesis of \945-chiral
-- bicyclo[1.1.1]pentanes", _publisher = "Springer Science and Business Media LLC", _authors =
-- Author {_given = Just "Marie L. J.", _family = "Wong"} :| [Author {_given = Just "Alistair J.",
-- _family = "Sterling"},Author {_given = Just "James J.", _family = "Mousseau"},Author {_given =
-- Just "Fernanda", _family = "Duarte"},Author {_given = Just "Edward A.", _family = "Anderson"}],
-- _journalLong = "Nature Communications", _journalShort = "Nat Commun", _year = 2021, _volume =
-- "12", _issue = "1", _pages = "", _doi = "10.1038/s41467-021-21936-4", _isbn = "", _articleNumber
-- = "1644"}
-- >>> natComm ^. journalShort   -- this is wrong, there should be periods
-- "Nat Commun"
-- >>> natComm & journalShort .~ "Nat. Commun."
-- Work {_workType = JournalArticle, _title = "Direct catalytic asymmetric synthesis of \945-chiral
-- bicyclo[1.1.1]pentanes", _publisher = "Springer Science and Business Media LLC", _authors =
-- Author {_given = Just "Marie L. J.", _family = "Wong"} :| [Author {_given = Just "Alistair J.",
-- _family = "Sterling"},Author {_given = Just "James J.", _family = "Mousseau"},Author {_given =
-- Just "Fernanda", _family = "Duarte"},Author {_given = Just "Edward A.", _family = "Anderson"}],
-- _journalLong = "Nature Communications", _journalShort = "Nat. Commun.", _year = 2021, _volume =
-- "12", _issue = "1", _pages = "", _doi = "10.1038/s41467-021-21936-4", _isbn = "", _articleNumber
-- = "1644"}

-- $crossref-exceptions
-- If 'fetchWork' and co. fail, then they will return a @Left
-- CrossrefException@. The possible exceptions are as follows. Often, the most
-- important question is which DOI led to an exception: this can be obtained
-- using 'getDoiFromException'. (You can also pattern-match on the
-- 'CrossrefException' value, but that's a bit verbose, and that's pretty much
-- what 'getDoiFromException' does for you anyway.)

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
