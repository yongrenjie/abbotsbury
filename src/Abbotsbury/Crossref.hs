module Abbotsbury.Crossref where


import           Abbotsbury.Work
import           Abbotsbury.Crossref.Internal


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
-- Note that this fixes "common" errors in Crossref's "short-container-title" entry (which is often
-- incorrect). If you don't want this to happen automatically, then use `fetchUnmodifiedWork`.
-- fetchWork uses a new Manager on each call. If you don't want this, then use
-- `fetchWorkWithManager`.
fetchWork
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWork = fetchWorkWithOptions Nothing defaultJournalFix


-- | The same as fetchWork, but doesn't attempt to fix Crossref's default "short-container-title".
-- fetchWork' uses a new Manager on each call. If you don't want this, then use
-- `fetchWork'`.
fetchWork'
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWork' = fetchWorkWithOptions Nothing emptyJournalFix


-- | The same as `fetchWork`, but uses a specific Manager.
fetchWorkWithManager
  :: NHC.Manager  -- ^ The http-client Manager. Note that this should be created using Network.Http.Client.TLS.tlsManagerSttings.
  -> Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWorkWithManager manager =
  fetchWorkWithOptions (Just manager) defaultJournalFix


-- | The same as `fetchWork'`, but uses a specific Manager.
fetchWorkWithManager'
  :: NHC.Manager  -- ^ The http-client Manager. Note that this should be created using Network.Http.Client.TLS.tlsManagerSttings.
  -> Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWorkWithManager' manager =
  fetchWorkWithOptions (Just manager) emptyJournalFix


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
