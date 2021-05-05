module Abbotsbury.Crossref where


import           Abbotsbury.Work


import           Control.Applicative            ( (<|>) )
import qualified Control.Exception             as CE
import           Data.Aeson
import qualified Data.Aeson.Types              as DAT
import           Data.Bifunctor                 ( first )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
-- import qualified Data.HashMap.Strict           as HM -- needs unordered-containers
import qualified Data.Text                     as T
import qualified Network.HTTP.Client           as HttpC
import           Network.HTTP.Req
-- import qualified Text.URI                      as URI


-- | The Crossref JSON schema is documented at
-- https://github.com/Crossref/rest-api-doc/blob/master/api_format.md.

-- | Fetching data from Crossref can fail for a number of reasons. In this case we either throw a
-- CrossrefException (from fetchCrossrefJson), or we put it
data CrossrefException
  = CRHttpException HttpC.HttpException  -- HTTP errors, e.g. no Internet connection.
  | CRJsonException Text                 -- Got the JSON from Crossref, but it was not valid JSON. The Text is from Aeson and says what went wrong.
  | CRUnknownWorkException WorkType      -- Abbotsbury right now only parses journal articles. If you request metadata about a book (for example) this error will be returned.
  | CROtherException Text                -- Something else went wrong.
  deriving Show


instance CE.Exception CrossrefException
-- | HttpC.HttpException doesn't have an Eq clause, so we just skip checking it.
instance Eq CrossrefException where
  (==) :: CrossrefException -> CrossrefException -> Bool
  CRHttpException        _  == CRHttpException        _  = True
  CRJsonException        t1 == CRJsonException        t2 = t1 == t2
  CRUnknownWorkException w1 == CRUnknownWorkException w2 = w1 == w2
  CROtherException       t1 == CROtherException       t2 = t1 == t2
  _                         == _                         = False


-- | Convert a DOI into a full-fledged Work by fetching metadata from Crossref.
--
-- In practice, Crossref data is fetched and parsed in a series of functions. In theory they could
-- easily be lumped into this one function, but it seems easier to organise them in smaller
-- functions.
-- Note that this fixes "common" errors in Crossref's "short-container-title" entry (which is often
-- incorrect). If you don't want this to happen automatically, then use `fetchUnmodifiedWork`.
fetchWork
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchWork email doi' = (fmap . fmap)
  (fixJournalShortInWork defaultJournalShortMap)
  (fetchUnmodifiedWork email doi')


-- | The same as fetchWork, but doesn't attempt to fix Crossref's default "short-container-title".
fetchUnmodifiedWork
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Work)
fetchUnmodifiedWork email doi' = do
  -- IO monad
  rawJsonOrError <- getCrossrefJson email doi'
  pure $ rawJsonOrError >>= getJsonMessage >>= parseCrossrefMessage


-- | Step 1 is to fetch the raw JSON response from Crossref for a given DOI. Errors that can occur
-- here are CRHttpException (if the HTTPS request fails) or CRJSONInvalidException (if a JSON
-- response is obtained but it is entirely invalid). As long as the JSON response is valid, we move
-- on to the next stage.
getCrossrefJson
  :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The DOI of interest.
  -> IO (Either CrossrefException Value)  -- ^ The raw JSON, represented as an Aeson Value.
getCrossrefJson email doi' = do
  -- IO monad
  eitherJsonResponse <- CE.try $ runReq defaultHttpConfig $ do
    -- Req monad
    let uri          = https "api.crossref.org" /: "works" /: doi'
    let httpsOptions = "mailto" =: email
    resp <- req GET uri NoReqBody jsonResponse httpsOptions
    pure (responseBody resp)
  -- Check for errors arising from Req and convert them to CrossrefException accordingly.
  pure $ case eitherJsonResponse of
    Left exc -> case exc of
      VanillaHttpException httpExc -> Left (CRHttpException httpExc)
      JsonHttpException    jsonExc -> Left (CRJsonException (T.pack jsonExc))
    Right val -> Right val


-- | Step 2 is to get the 'message' component.
getJsonMessage
  :: Value -- ^ The raw JSON from Crossref.
  -> Either CrossrefException Value
getJsonMessage val = do
  let aesonParseResult =
        DAT.parseEither (withObject "Crossref JSON" (.: "message")) val
  first (CRJsonException . T.pack) aesonParseResult


-- | Step 3a is to identify the type of work. This basically gives a CRUnknownWorkException if it's
-- not an article, which is technically a bug, but I don't know when I'll be able to work on adding
-- new types of works (e.g. books).
identifyWorkType :: Value -> Either CrossrefException WorkType
identifyWorkType messageVal = do
  let parsedWorkType =
        DAT.parseEither (withObject "Crossref response" (.: "type")) messageVal
  case parsedWorkType of
    Left failedParseMessage ->
      Left (CRJsonException (T.pack failedParseMessage))
    Right wType -> case wType of
      "book-section"        -> Right BookSection
      "monograph"           -> Right Monograph
      "report"              -> Right Report
      "peer-review"         -> Right PeerReview
      "book-track"          -> Right BookTrack
      "journal-article"     -> Right JournalArticle
      "book-part"           -> Right Part
      "other"               -> Right Other
      "book"                -> Right Book
      "journal-volume"      -> Right JournalVolume
      "book-set"            -> Right BookSet
      "reference-entry"     -> Right ReferenceEntry
      "proceedings-article" -> Right ProceedingsArticle
      "journal"             -> Right Journal
      "component"           -> Right Component
      "book-chapter"        -> Right BookChapter
      "proceedings-series"  -> Right ProceedingsSeries
      "report-series"       -> Right ReportSeries
      "proceedings"         -> Right Proceedings
      "standard"            -> Right Standard
      "reference-book"      -> Right ReferenceBook
      "posted-content"      -> Right PostedContent
      "journal-issue"       -> Right JournalIssue
      "dissertation"        -> Right Dissertation
      "dataset"             -> Right Dataset
      "book-series"         -> Right BookSeries
      "edited-book"         -> Right EditedBook
      "standard-series"     -> Right StandardSeries
      _                     -> Left
        (  CRJsonException
        $  "work type '"
        <> T.pack wType
        <> "' not found in Crossref schema. "
        <> "This should probably be reported to the Crossref maintainers."
        )


-- | Step 3b is to parse the 'message' component into the Abbotsbury data types, depending on which type
-- of work it is. Right now only Articles are supported.
parseCrossrefMessage
  :: Value -- ^ The 'message' component of the Crossref JSON data.
  -> Either CrossrefException Work  -- Either an error message or the Work.
parseCrossrefMessage messageVal = do
  -- Figure out which parser to use.
  wType  <- identifyWorkType messageVal
  parser <- case wType of
    JournalArticle -> Right parseJournalArticle
    _              -> Left (CRUnknownWorkException wType)
  -- Run the appropriate parser on the message Value.
  let parsedWork =
        DAT.parseEither (withObject "Crossref response" parser) messageVal
  first (CRJsonException . T.pack) parsedWork


-- | The parser which parses JournalArticles.
parseJournalArticle :: Object -> DAT.Parser Work
parseJournalArticle messageObj = do
  -- Title
  let _workType = JournalArticle
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
  pure $ Work { .. }


safeHead
  :: String -- ^  An error message to display if the list was empty.
  -> DAT.Parser [a] -- ^ An Aeson parser which returns a list of results.
  -> DAT.Parser a   -- ^ An Aeson parser which returns the first item.
safeHead errorMsg listParser = do
  list <- listParser
  case list of
    []      -> fail errorMsg
    (x : _) -> pure x


-- This is almost the same as the automatically derived parseJSON method, but with two differences:
-- (1) We prefixed the record fields with underscores, which we need to account for.
-- (2) We force initials in the given name to be separated by spaces (this is partly for consistency
-- but also ensures that names are parsed correctly for citation generation). For example, the given
-- name "A.B." is modified to "A. B.".
parseAuthor
  :: Object -- ^ The Crossref JSON for an author.
  -> DAT.Parser Author
parseAuthor authJSON = do
  _given  <- (fmap . fmap) separateInitials (authJSON .:? "given")
  _family <- authJSON .: "family"
  pure Author { .. }


-- | This function enforces the separation of initials by a space, as described in parseAuthor.
separateInitials :: Text -> Text
separateInitials t = T.concat . zipWith f [1 ..] . T.unpack $ t
 where
      -- We only want to add a space after a dot, and if the dot isn't (1) at the end of the name;
      -- (2) followed by a hyphen; or (3) already followed by a space.
  f :: Int -> Char -> Text
  f index c = if c == '.' && notElem nextChar [Nothing, Just '-', Just ' ']
    then ". "
    else T.singleton c
   where
    nextChar = case T.drop index t of
      "" -> Nothing
      xs -> Just (T.head xs)


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
fixJournalShortInWork m work = work { _journalShort = right_journalShort }
 where
  wrong_journalShort = _journalShort work
  right_journalShort = fixJournalShort m wrong_journalShort


-- | A predefined list of (actual, expected) journal short names which can be used as the argument
-- to fixJournalShort and fixJournalShortInWork. These come up in my own work.
defaultJournalShortMap :: Map Text Text
defaultJournalShortMap = M.fromList
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
