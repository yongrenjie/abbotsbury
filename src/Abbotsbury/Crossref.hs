module Abbotsbury.Crossref where


import           Abbotsbury.Work
import           Abbotsbury.Crossref.Internal


import           Control.Concurrent
import           Control.Monad
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Network.HTTP.Client           as NHC
import qualified Network.HTTP.Client.TLS       as NHCT


-- The Crossref JSON schema is documented at
-- https://github.com/Crossref/rest-api-doc/blob/master/api_format.md.


-- | Convert a DOI into a full-fledged Work by fetching metadata from Crossref.
--
-- In practice, Crossref data is fetched and parsed in a series of functions. In theory they could
-- easily be lumped into this one function, but it seems easier to organise them in smaller
-- functions.
-- Note that this fixes "common" errors in Crossref's "short-container-title" entry (which are often
-- incorrect), according to a hardcoded Map of journal title replacements (see `defaultJournalFix`).
-- Also, this function creates a new HTTP manager every time it is called. This is meant to be
-- simple default behaviour: if you want something different, you can use `fetchWorkWithOptions` and
-- pass appropriate arguments.
fetchWork
  :: Text -- ^ Your email address. This is mandatory for making a polite request to the Crossref API.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWork = fetchWorkWithOptions Nothing defaultJournalFix


-- | Generalised version of fetchWork.
fetchWorkWithOptions
  :: Maybe NHC.Manager  -- ^ Just a Manager if a specific one is to be used. Nothing if a new one is to be created.
  -> Map Text Text  -- ^ Map of (actual short journal name, expected short journal name).
  -> Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWorkWithOptions maybeManager fixMap email doi' = do
  -- Set up the manager. If it's not specified, create a new one using default settings.
  manager <- case maybeManager of
    Nothing -> NHC.newManager NHCT.tlsManagerSettings
    Just m  -> pure m
  -- Get the JSON data.
  eitherErrorJson <- getCrossrefJson manager email doi'
  -- Parse the JSON data.
  let eitherErrorWork =
        eitherErrorJson >>= getJsonMessage >>= parseCrossrefMessage
  -- Perform journal replacements
  pure $ fmap (fixJournalShortInWork fixMap) eitherErrorWork


-- | The same as `fetchWork`, but concurrently fetches metadata for a series of DOIs (it uses the
-- same HTTP manager for all DOIs).
fetchWorks
  :: Text -- ^ Your email address. This is mandatory for making a polite request to the Crossref API.
  -> [DOI] -- ^ The DOI of interest.
  -> IO [Either CrossrefException Work]
fetchWorks = fetchWorksWithOptions Nothing defaultJournalFix


-- | The same as `fetchWorkWithOptions`, but concurrently fetches metadata for a series of DOIs (it
-- uses the same HTTP manager for all DOIs).
fetchWorksWithOptions
  :: Maybe NHC.Manager  -- ^ Just a Manager if a specific one is to be used. Nothing if a new one is to be created.
  -> Map Text Text  -- ^ Map of (actual short journal name, expected short journal name).
  -> Text -- ^ Your email address. This is mandatory for making a polite request.
  -> [DOI] -- ^ The DOIs of interest.
  -> IO [Either CrossrefException Work]
fetchWorksWithOptions maybeManager fixMap email dois = if null dois
  then pure []   -- Avoid doing more work than we need to.
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
        -- Parse the JSON data.
        let eitherErrorWork =
              eitherErrorJson >>= getJsonMessage >>= parseCrossrefMessage
        -- Perform journal replacements and return it to the MVar.
        putMVar mvar $ fmap (fixJournalShortInWork fixMap) eitherErrorWork
      )
    mapM takeMVar mvars


-- | A predefined list of (actual, expected) journal short names which can be used as the argument
-- to fixJournalShort and fixJournalShortInWork. These come up in my own work.
defaultJournalFix :: Map Text Text
defaultJournalFix = M.fromList
  [ ( "Proceedings of the National Academy of Sciences"
    , "Proc. Natl. Acad. Sci. U. S. A."
    )
  , ("The Journal of Chemical Physics"     , "J. Chem. Phys.")
  , ("Journal of Magnetic Resonance"       , "J. Magn. Reson.")
  , ("Journal of Magnetic Resonance (1969)", "J. Magn. Reson.")
  , ( "Progress in Nuclear Magnetic Resonance Spectroscopy"
    , "Prog. Nucl. Magn. Reson. Spectrosc."
    )
  , ("Magn Reson Chem"                   , "Magn. Reson. Chem.")
  , ("Chemical Physics Letters"          , "Chem. Phys. Lett.")
  , ("Biochemistry Journal"              , "Biochem. J.")
  , ("Journal of Magnetic Resonance, Series A", "J. Magn. Reson., Ser. A")
  , ("Journal of Magnetic Resonance, Series B", "J. Magn. Reson., Ser. B")
  , ("J Biomol NMR"                      , "J. Biomol. NMR")
  , ("Annual Reports on NMR Spectroscopy", "Annu. Rep. NMR Spectrosc.")
  , ("Angewandte Chemie International Edition", "Angew. Chem. Int. Ed.")
  , ("Nat Commun"                        , "Nat. Commun.")
  , ("Sci Rep"                           , "Sci. Rep.")
  , ("Nucleic Acids Research"            , "Nucleic Acids Res.")
  , ("Journal of Molecular Biology"      , "J. Mol. Biol.")
  , ("Journal of Chemical Informatics and Modeling", "J. Chem. Inf. Model.")
  , ("Journal of Computational Chemistry", "J. Comp. Chem.")
  , ("Nat Rev Methods Primers"           , "Nat. Rev. Methods Primers")
  ]


-- | Convenience function for users.
emptyJournalFix :: Map Text Text
emptyJournalFix = M.empty
