-- |
-- Module    : Abbotsbury.Crossref
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This module, and its submodules, provide the functionality which allows
-- fetching metadata from Crossref and parsing them into 'Work's. All of the
-- functionality here is re-exported by the top-level module ("Abbotsbury").
module Abbotsbury.Crossref
  ( -- * Getting and parsing raw JSON data
    -- $crossref-parse
    -- $crossref-journalfix
    fetchWork
  , fetchWork'
  , fetchWorks
  , fetchWorks'
    -- * What could go wrong
    -- $crossref-exceptions
  , Abbotsbury.Crossref.Internal.CrossrefException
  , Abbotsbury.Crossref.Internal.getDoiFromException
  ) where

import           Abbotsbury.Crossref.Internal
import           Abbotsbury.Work
import           Control.Concurrent
import           Control.Monad
import           Data.Text                      ( Text )
import qualified Network.HTTP.Client           as NHC
import qualified Network.HTTP.Client.TLS       as NHCT

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
-- >>> Right natComm <- fetchWork' Nothing False "your@email.com" "10.1038/s41467-021-21936-4"
-- >>> natComm
-- IsJournalArticle (JournalArticle {_title = "Direct catalytic asymmetric synthesis of \945-chiral 
-- bicyclo[1.1.1]pentanes", _authors = Author {_given = Just "Marie L. J.", _family = "Wong"} :| [Au
-- thor {_given = Just "Alistair J.", _family = "Sterling"},Author {_given = Just "James J.", _famil
-- y = "Mousseau"},Author {_given = Just "Fernanda", _family = "Duarte"},Author {_given = Just "Edwa
-- rd A.", _family = "Anderson"}], _journalLong = "Nature Communications", _journalShort = "Nat Comm
-- un", _year = 2021, _volume = "12", _issue = "1", _pages = "", _doi = "10.1038/s41467-021-21936-4"
-- , _articleNumber = "1644"})
-- >>> let (IsJournalArticle ja) = natComm
-- >>> _journalShort ja  -- the result of this is wrong: there should be periods
-- "Nat Commun"
-- >>> IsJournalArticle (ja { _journalShort = "Nat. Commun." })
-- IsJournalArticle (JournalArticle {_title = "Direct catalytic asymmetric synthesis of \945-chiral 
-- bicyclo[1.1.1]pentanes", _authors = Author {_given = Just "Marie L. J.", _family = "Wong"} :| [Au
-- thor {_given = Just "Alistair J.", _family = "Sterling"},Author {_given = Just "James J.", _famil
-- y = "Mousseau"},Author {_given = Just "Fernanda", _family = "Duarte"},Author {_given = Just "Edwa
-- rd A.", _family = "Anderson"}], _journalLong = "Nature Communications", _journalShort = "Nat. Com
-- mun.", _year = 2021, _volume = "12", _issue = "1", _pages = "", _doi = "10.1038/s41467-021-21936-
-- 4", _articleNumber = "1644"})

-- | Convert a DOI into a full-fledged Work by fetching metadata from Crossref.
--
-- Note that this function uses a hardcoded @Map@ of journal title abbreviations
-- to obtain the abbreviated journal name, thus bypassing Crossref's
-- @short-container-title@ entries (which are sometimes incorrect). It falls
-- back on Crossref's @short-container-title@ if the @Map@ doesn't contain an
-- entry for the journal in question, and if that doesn't exist, it uses the
-- full journal title as the last fallback. If you don't want this behaviour,
-- use 'fetchWork'' and pass @False@ as the second argument.
--
-- Also, this function creates a new HTTP 'Network.HTTP.Client.Manager' every
-- time it is called. Again, if you don't want this, 'fetchWork'' lets you
-- customise this.
--
-- Assuming you have a working Internet connection:
--
-- >>> nrmp <- fetchWork "your@email.com" "10.1038/s43586-021-00024-3"
-- >>> nrmp
-- Right (Work {_workType = JournalArticle, _title = "Parallel nuclear magnetic
-- resonance spectroscopy", _authors = Author {_given = Just "\274riks", _family
-- = "Kup\269e"} :| [Author {_given = Just "Lucio", _family = "Frydman"},Author
-- {_given = Just "Andrew G.", _family = "Webb"},Author {_given = Just "Jonathan
-- R. J.", _family = "Yong"},Author {_given = Just "Tim D. W.", _family =
-- "Claridge"}], _journalLong = "Nature Reviews Methods Primers", _journalShort
-- = "Nat. Rev. Methods Primers", _year = 2021, _volume = "1", _issue = "1",
-- _pages = "", _doi = "10.1038/s43586-021-00024-3", _isbn = "", _articleNumber
-- = "27"})
-- >>> nope <- fetchWork "your@email.com" "this_doi_doesnt_exist"
-- >>> nope
-- Left (CRHttpException "this_doi_doesnt_exist" (HttpExceptionRequest Request {
--    (long HTTP exception output elided...)
-- }) "Resource not found.")))
--
-- The implementation of this function calls on a number of smaller functions,
-- which are described in "Abbotsbury.Crossref.Internal".
fetchWork
  ::
  -- | Your email address. This is mandatory for making a polite request to the
  -- Crossref API.
     Text
  ->
  -- | The DOI of interest. @abbotsbury@ only really works with journal
  -- articles, for which DOIs are a good identifier. Some books have DOIs (and
  -- in fact it is possible to search Crossref by ISBN), but the metadata
  -- available on Crossref for books is quite patchy. Hence, for now, this only
  -- really works with DOIs.
     DOI
  -> IO (Either CrossrefException Work)
fetchWork = fetchWork' Nothing True

-- | The same as 'fetchWork', but concurrently fetches metadata for a series of
-- DOIs (it uses the same 'Network.HTTP.Client.Manager' for all DOIs).
fetchWorks
  ::
  -- | Your email address. This is mandatory for making a polite request to the
  -- Crossref API.
     Text
  ->
  -- | The DOIs of interest.
     [DOI]
  -> IO [Either CrossrefException Work]
fetchWorks = fetchWorks' Nothing True

-- | Generalised version of 'fetchWork', which allows the user to specify the
-- @http-client@ 'Network.HTTP.Client.Manager' used for the request, and also
-- control which incorrect Crossref entries are fixed.
fetchWork'
  ::
  -- | @Just manager@ if a specific one is to be used. @Nothing@ if a new one is
  -- to be created.
     Maybe NHC.Manager
  ->
  -- | @True@ if @abbotsbury@'s internal abbreviation list is to be used (which
  -- falls back on Crossref metadata anyway). @False@ if Crossref data is to be
  -- used as-is.
     Bool
  ->
  -- | Your email address. This is mandatory for making a polite request.
     Text
  ->
  -- | The DOI of interest.
     DOI
  -> IO (Either CrossrefException Work)
fetchWork' maybeManager useInternalAbbrevs email doi' = do
  -- Set up the manager. If it's not specified, create a new one using default
  -- settings.
  manager <- case maybeManager of
    Nothing -> NHC.newManager NHCT.tlsManagerSettings
    Just m  -> pure m
  -- Get the JSON data.
  eitherErrorJson <- getCrossrefJson manager email doi'
  -- Parse the JSON data. (Note that the argument to 'pure' runs in the Either
  -- monad, not IO.)
  pure
    $   eitherErrorJson
    >>= getJsonMessage doi'
    >>= parseCrossrefMessage doi' useInternalAbbrevs

-- | The same as 'fetchWork'', but concurrently fetches metadata for a series of
-- DOIs (it uses the same HTTP manager for all DOIs).
fetchWorks'
  ::
  -- | @Just manager@ if a specific one is to be used. @Nothing@ if a new one is
  -- to be created.
     Maybe NHC.Manager
  ->
  -- | @True@ if @abbotsbury@'s internal abbreviation list is to be used (which
  -- falls back on Crossref metadata anyway). @False@ if Crossref data is to be
  -- used as-is.
     Bool
  ->
  -- | Your email address. This is mandatory for making a polite request.
     Text
  ->
  -- | The DOIs of interest.
     [DOI]
  -> IO [Either CrossrefException Work]
fetchWorks' maybeManager useInternalAbbrevs email dois = if null dois
  then pure [] -- Avoid doing more work than we need to.
  else do
                                                 -- Set up the manager. If it's not specified, create a new one using default settings.
    manager <- case maybeManager of
      Nothing -> NHC.newManager NHCT.tlsManagerSettings
      Just m  -> pure m
    -- Create a bunch of mvars.
    let n = length dois
    mvars <- replicateM n newEmptyMVar
    -- Fetch metadata concurrently.
    forM_
      (zip mvars dois)
      (\(mvar, doi') -> forkIO $ do
          -- Get the JSON data.
        eitherErrorJson <- getCrossrefJson manager email doi'
        -- Parse the JSON data. (Note that the argument to 'pure' runs in the
        -- Either monad, not IO.)
        let eitherErrorWork =
              eitherErrorJson
                >>= getJsonMessage doi'
                >>= parseCrossrefMessage doi' useInternalAbbrevs
        putMVar mvar eitherErrorWork
      )
    mapM takeMVar mvars

-- $crossref-exceptions
-- If 'fetchWork' and co. fail, then they will return a @Left
-- CrossrefException@. The possible exceptions are as follows. Often, the most
-- important question is which DOI led to an exception: this can be obtained
-- using 'getDoiFromException'. (You can also pattern-match on the
-- 'CrossrefException' value, but that's a bit verbose, and that's pretty much
-- what 'getDoiFromException' does for you anyway.)

