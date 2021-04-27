module Abbot.Crossref where


import Abbot.Work


import           Control.Applicative            ( (<|>) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Aeson
import qualified Data.Aeson.Types              as DAT
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
-- import qualified Data.HashMap.Strict           as HM -- needs unordered-containers
import qualified Data.Text                     as T
import           Network.HTTP.Req
-- import qualified Text.URI                      as URI


-- | Fetch the raw JSON response from Crossref for a given DOI.
-- Note that this function doesn't check that the JSON is valid at all.
-- TODO: How to encode possible failures?
getCrossrefJSON
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The desired DOI.
  -> IO Value
getCrossrefJSON email doi' = runReq defaultHttpConfig $ do
  resp <- req GET (https "api.crossref.org" /: "works" /: doi') NoReqBody jsonResponse $
    "mailto" =: email
  pure (responseBody resp)


identifyCrossrefWorkType :: Value -> Maybe Text
identifyCrossrefWorkType = DAT.parseMaybe $ withObject "Crossref response" getCRObjectType
  where
    getCRObjectType :: Object -> DAT.Parser Text
    getCRObjectType obj = do
      messageObj <- obj .: "message"   -- messageObj :: Object
      messageObj .: "type"


parseCrossrefValue
  :: Value -- ^ The raw JSON returned by Crossref.
  -> Either Text Work  -- Either an error message or the Work.
parseCrossrefValue val = convESWtoETW
  $ DAT.parseEither (withObject "Crossref response" parser) val
 where
  -- Change the error type of Either.
  convESWtoETW :: Either String Work -> Either Text Work
  convESWtoETW (Left  s) = Left (T.pack s)
  convESWtoETW (Right w) = Right w
  -- The actual parser.
  parser :: Object -> DAT.Parser Work
  parser = case identifyCrossrefWorkType val of
    Just "journal-article" -> parseArticle
    Just other ->
      error . T.unpack $ ("resource type " <> other <> " not implemented")
    Nothing -> error "resource type not found"


parseArticle :: Object -> DAT.Parser Work
parseArticle obj = do
  messageObj   <- obj .: "message"
  -- Title
  _title       <- safeHead "could not get title" $ messageObj .: "title"
  -- Authors
  _authorArray <- messageObj .: "author"
  _authors     <- do
    auths <- mapM parseAuthor _authorArray
    case NE.nonEmpty auths of
      Just x  -> pure x
      Nothing -> fail "expected at least one author, found none"
  -- Journal names
  _journalLong <-
    safeHead "could not get journal title from container-title key"
    $  messageObj
    .: "container-title"
  _journalShort <- safeHead "" (messageObj .: "short-container-title")
    <|> pure _journalLong
  -- Year (prefer print publish date over online publish date as the former is the one usually used)
  publishedObj <-
    messageObj .: "published-print" <|> messageObj .: "published-online"
  _year <- safeHead "year not found in date-parts"
    $ safeHead "date-parts was empty" (publishedObj .: "date-parts")
  -- Volume, issue, pages, DOI
  _volume <- messageObj .:? "volume" .!= ""
  _issue  <- (messageObj .: "journal-issue" >>= (.: "issue")) <|> pure ""
  _pages  <- messageObj .:? "page" .!= ""
  _doi    <- messageObj .:? "DOI" .!= ""
  pure Article { .. }


safeHead
  :: String -- ^  An error message to display if the list was empty.
  -> DAT.Parser [a] -- ^ An Aeson parser which returns a list of results.
  -> DAT.Parser a   -- ^ An Aeson parser which returns the first item.
safeHead errorMsg listParser = do
  list <- listParser
  case list of
    []      -> fail errorMsg
    (x : _) -> pure x


-- This is almost the same as the automatically derived parseJSON method, but because we prefixed
-- the record fields with underscores, we have to write it ourselves.
parseAuthor
  :: Object -- ^ The Crossref JSON for an author.
  -> DAT.Parser Author
parseAuthor authJSON = do
  _given  <- authJSON .:? "given"
  _family <- authJSON .: "family"
  pure Author { .. }


-- | "Short" journal names should be abbreviated according to the Chemical Abstracts Service Source
-- Index (CASSI), accessible at https://cassi.cas.org/. However, Crossref JSON data occasionally:
-- (1) does not have the "short-container-title" entry, meaning that no short journal name is
-- provided (in this case abbotsbury uses the full name); or
-- (2) has incorrect or unhelpful entries in this field, which lead to incorrect citations.
--
-- In both cases the actual "short name" that abbotsbury finds is not correct. This function
-- attempts to correct for that. It's important that the keys of the Map are the **actual short
-- names** that abbotsbury provides. For example, if you expected to get "Nature Chem." but got
-- "Nat. Chem.", then pass `Map.fromList [("Nat. Chem.", "Nature Chem.")] as the first argument.
fixJournalShort
  :: Map Text Text   -- ^ A Map of (actual short name, expected short name) pairs.
  -> Text -- ^ The actual "short journal name" that abbotsbury found.
  -> Text -- ^ The correct name if the actual name was found in the Map; otherwise the actual name.
fixJournalShort m wrong = fromMaybe wrong (m M.!? wrong)


-- | The same as fixJournalShort, but can be applied to an entire Work. This simply returns the
-- original Work if the Work does not have a _journalShort attribute.
fixJournalShortInWork :: Map Text Text -> Work -> Work
fixJournalShortInWork m work = case work of
  Article{} -> work { _journalShort = right_journalShort }
  -- _         -> work  -- uncomment when we have more types
 where
  wrong_journalShort = _journalShort work
  right_journalShort = fixJournalShort m wrong_journalShort


-- | A predefined list of (actual, expected) journal short names which can be used as the argument
-- to fixJournalShort and fixJournalShortInWork. These come up in my own work.
defaultJournalShortMap :: Map Text Text
defaultJournalShortMap = M.fromList
  [ ("Proceedings of the National Academy of Sciences", "Proc. Natl. Acad. Sci. U. S. A.")
  , ("The Journal of Chemical Physics", "J. Chem. Phys.")
  , ("Journal of Magnetic Resonance", "J. Magn. Reson.")
  , ("Journal of Magnetic Resonance (1969)", "J. Magn. Reson.")
  , ("Progress in Nuclear Magnetic Resonance Spectroscopy", "Prog. Nucl. Magn. Reson. Spectrosc.")
  , ("Magn Reson Chem", "Magn. Reson. Chem.")
  , ("Chemical Physics Letters", "Chem. Phys. Lett.")
  , ("Biochemistry Journal", "Biochem. J.")
  , ("Journal of Magnetic Resonance, Series A", "J. Magn. Reson., Ser. A")
  , ("Journal of Magnetic Resonance, Series B", "J. Magn. Reson., Ser. B")
  , ("J Biomol NMR", "J. Biomol. NMR")
  , ("Annual Reports on NMR Spectroscopy", "Annu. Rep. NMR Spectrosc.")
  , ("Angewandte Chemie International Edition", "Angew. Chem. Int. Ed.")
  , ("Nat Commun", "Nat. Commun.")
  , ("Sci Rep", "Sci. Rep.")
  , ("Nucleic Acids Research", "Nucleic Acids Res.")
  , ("Journal of Molecular Biology", "J. Mol. Biol.")
  , ("Journal of Chemical Informatics and Modeling", "J. Chem. Inf. Model.")
  , ("Journal of Computational Chemistry", "J. Comp. Chem.")
  , ("Nat Rev Methods Primers", "Nat. Rev. Methods Primers")
  ]

