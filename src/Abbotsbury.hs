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
  ( Abbotsbury.Crossref.fetchWork,
    Abbotsbury.Crossref.fetchWorks,
    Abbotsbury.Crossref.fetchWorkWithOptions,
    Abbotsbury.Crossref.fetchWorksWithOptions,
    Abbotsbury.Crossref.defaultJournalFix,
    Abbotsbury.Crossref.emptyJournalFix,
    Abbotsbury.Cite.cite,
    Abbotsbury.Crossref.Internal.CrossrefException (..),
    Abbotsbury.Crossref.Internal.getDoiFromException,
    Abbotsbury.Cite.Style (..),
    Abbotsbury.Cite.acsStyle,
    Abbotsbury.Cite.bibStyle,
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
