-- | This module provides the functionality which allows fetching metadata from
-- Crossref and parsing them into 'Work's.
--
-- Note that all the @fetchWork...@ functions take an extra @Text@ argument,
-- which is supposed to be your email address. The reason for this is
-- politeness. Crossref asks that users of their API provide some form of
-- contact information in their requests:
-- <https://github.com/CrossRef/rest-api-doc#etiquette>
-- The positive side of this is that you get redirected to a 'polite' pool,
-- which is [slightly faster and more reliable](https://status.crossref.org/).
module Abbotsbury.Crossref
  ( -- $exceptions_note
    Abbotsbury.Crossref.Internal.CrossrefException,
    Abbotsbury.Crossref.Internal.getDoiFromException,
    fetchWork,
    fetchWorkWithOptions,
    fetchWorks,
    fetchWorksWithOptions,
    defaultJournalFix,
    emptyJournalFix,
  )
where

import Abbotsbury.Crossref.Internal
import Abbotsbury.Work
import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Network.HTTP.Client as NHC
import qualified Network.HTTP.Client.TLS as NHCT

-- $exceptions_note
-- The following functions :w
--

-- | Convert a DOI into a full-fledged Work by fetching metadata from Crossref.
--
-- Note that this function uses a hardcoded Map of journal title replacements
-- (see 'defaultJournalFix') to fix common errors in Crossref's
-- @short-container-title@ entries (which are often incorrect).  Also, this
-- function creates a new HTTP manager every time it is called. This is meant to
-- be simple default behaviour: if you want something different, you can use
-- 'fetchWorkWithOptions' and pass appropriate arguments.
--
-- Assuming you have a working Internet connection:
--
-- >>> nrmp <- fetchWork "your@email.com" "10.1038/s43586-021-00024-3"
-- >>> nrmp
-- Right (Work {_workType = JournalArticle, _title = "Parallel nuclear magnetic resonance spectroscopy", _authors = Author {_given = Just "\274riks", _family = "Kup\269e"} :| [Author {_given = Just "Lucio", _family = "Frydman"},Author {_given = Just "Andrew G.", _family = "Webb"},Author {_given = Just "Jonathan R. J.", _family = "Yong"},Author {_given = Just "Tim D. W.", _family = "Claridge"}], _journalLong = "Nature Reviews Methods Primers", _journalShort = "Nat. Rev. Methods Primers", _year = 2021, _volume = "1", _issue = "1", _pages = "", _doi = "10.1038/s43586-021-00024-3", _isbn = "", _articleNumber = "27"})
-- >>> nope <- fetchWork "your@email.com" "this_doi_doesnt_exist"
-- >>> nope
-- Left (CRHttpException "this_doi_doesnt_exist" (HttpExceptionRequest Request {
--    (long HTTP exception output elided...)
-- }) "Resource not found.")))
--
-- In practice, this function calls on a number of smaller functions, which are
-- described in "Abbotsbury.Crossref.Internal".
fetchWork ::
  -- | Your email address. This is mandatory for making a polite request to the
  -- Crossref API.
  Text ->
  -- | The DOI of interest. @abbotsbury@ only really works with journal
  -- articles, for which DOIs are a good identifier. Some books have DOIs (and
  -- in fact it is possible to search Crossref by ISBN), but the metadata
  -- available on Crossref for books is quite patchy. Hence, for now, this only
  -- really works with DOIs.
  DOI ->
  IO (Either CrossrefException Work)
fetchWork = fetchWorkWithOptions Nothing defaultJournalFix

-- | Generalised version of fetchWork.
fetchWorkWithOptions ::
  -- | Just a Manager if a specific one is to be used. Nothing if a new one is to be created.
  Maybe NHC.Manager ->
  -- | Map of (actual short journal name, expected short journal name).
  Map Text Text ->
  -- | Your email address. This is mandatory for making a polite request.
  Text ->
  -- | The DOI of interest.
  DOI ->
  IO (Either CrossrefException Work)
fetchWorkWithOptions maybeManager fixMap email doi' = do
  -- Set up the manager. If it's not specified, create a new one using default settings.
  manager <- case maybeManager of
    Nothing -> NHC.newManager NHCT.tlsManagerSettings
    Just m -> pure m
  -- Get the JSON data.
  eitherErrorJson <- getCrossrefJson manager email doi'
  -- Parse the JSON data.
  let eitherErrorWork =
        eitherErrorJson >>= getJsonMessage doi' >>= parseCrossrefMessage doi'
  -- Perform journal replacements
  pure $ fmap (fixJournalShortInWork fixMap) eitherErrorWork

-- | The same as `fetchWork`, but concurrently fetches metadata for a series of DOIs (it uses the
-- same HTTP manager for all DOIs).
fetchWorks ::
  -- | Your email address. This is mandatory for making a polite request to the Crossref API.
  Text ->
  -- | The DOI of interest.
  [DOI] ->
  IO [Either CrossrefException Work]
fetchWorks = fetchWorksWithOptions Nothing defaultJournalFix

-- | The same as `fetchWorkWithOptions`, but concurrently fetches metadata for a series of DOIs (it
-- uses the same HTTP manager for all DOIs).
fetchWorksWithOptions ::
  -- | Just a Manager if a specific one is to be used. Nothing if a new one is to be created.
  Maybe NHC.Manager ->
  -- | Map of (actual short journal name, expected short journal name).
  Map Text Text ->
  -- | Your email address. This is mandatory for making a polite request.
  Text ->
  -- | The DOIs of interest.
  [DOI] ->
  IO [Either CrossrefException Work]
fetchWorksWithOptions maybeManager fixMap email dois =
  if null dois
    then pure [] -- Avoid doing more work than we need to.
    else do
      -- Set up the manager. If it's not specified, create a new one using default settings.
      manager <- case maybeManager of
        Nothing -> NHC.newManager NHCT.tlsManagerSettings
        Just m -> pure m
      -- Create a bunch of mvars.
      let n = length dois
      mvars <- replicateM n newEmptyMVar
      -- Fetch metadata concurrently.
      forM_
        (zip mvars dois)
        ( \(mvar, doi') -> forkIO $ do
            -- Get the JSON data.
            eitherErrorJson <- getCrossrefJson manager email doi'
            -- Parse the JSON data.
            let eitherErrorWork =
                  eitherErrorJson >>= getJsonMessage doi' >>= parseCrossrefMessage doi'
            -- Perform journal replacements and return it to the MVar.
            putMVar mvar $ fmap (fixJournalShortInWork fixMap) eitherErrorWork
        )
      mapM takeMVar mvars

-- | A predefined list of (actual, expected) journal short names which can be used as the argument
-- to fixJournalShort and fixJournalShortInWork. These come up in my own work.
defaultJournalFix :: Map Text Text
defaultJournalFix =
  M.fromList
    [ ( "Proceedings of the National Academy of Sciences",
        "Proc. Natl. Acad. Sci. U. S. A."
      ),
      ("The Journal of Chemical Physics", "J. Chem. Phys."),
      ("Journal of Magnetic Resonance", "J. Magn. Reson."),
      ("Journal of Magnetic Resonance (1969)", "J. Magn. Reson."),
      ( "Progress in Nuclear Magnetic Resonance Spectroscopy",
        "Prog. Nucl. Magn. Reson. Spectrosc."
      ),
      ("Magn Reson Chem", "Magn. Reson. Chem."),
      ("Chemical Physics Letters", "Chem. Phys. Lett."),
      ("Biochemistry Journal", "Biochem. J."),
      ("Journal of Magnetic Resonance, Series A", "J. Magn. Reson., Ser. A"),
      ("Journal of Magnetic Resonance, Series B", "J. Magn. Reson., Ser. B"),
      ("J Biomol NMR", "J. Biomol. NMR"),
      ("Annual Reports on NMR Spectroscopy", "Annu. Rep. NMR Spectrosc."),
      ("Angewandte Chemie International Edition", "Angew. Chem. Int. Ed."),
      ("Nat Commun", "Nat. Commun."),
      ("Sci Rep", "Sci. Rep."),
      ("Nucleic Acids Research", "Nucleic Acids Res."),
      ("Journal of Molecular Biology", "J. Mol. Biol."),
      ("Journal of Chemical Informatics and Modeling", "J. Chem. Inf. Model."),
      ("Journal of Computational Chemistry", "J. Comp. Chem."),
      ("Nat Rev Methods Primers", "Nat. Rev. Methods Primers")
    ]

-- | Convenience function for users.
emptyJournalFix :: Map Text Text
emptyJournalFix = M.empty
