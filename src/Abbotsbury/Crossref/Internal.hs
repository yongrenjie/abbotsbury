module Abbotsbury.Crossref.Internal where

import           Abbotsbury.Work
import           Control.Applicative            ( (<|>) )
import qualified Control.Exception             as CE
import           Data.Aeson
import qualified Data.Aeson.Types              as DAT
import           Data.Bifunctor                 ( first )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Normalize            ( NormalizationMode(NFC)
                                                , normalize
                                                )
import qualified Network.HTTP.Client           as NHC
import           Network.URI                    ( escapeURIString
                                                , isUnescapedInURIComponent
                                                )

-- | Fetching data from Crossref can fail for a number of reasons. In this case we either throw a
-- CrossrefException (from fetchCrossrefJson), or we put it
data CrossrefException
  -- | HTTP errors, which includes non-404 responses (i.e. DOI not found).
  = CRHttpException DOI NHC.HttpException
  -- | Got the JSON from Crossref, but it was not valid JSON. The @Text@ is
  -- provided by Aeson and says what went wrong.
  | CRJsonException DOI Text
  -- | @abbotsbury@ right now only parses journal articles. If you request
  -- metadata about a book chapter (for example), this exception will be
  -- returned (the @Text@ value will contain the offending work type).
  | CRUnknownWorkException DOI Text
  -- | Something else went wrong.
  | CROtherException DOI Text
  deriving (Show)

instance CE.Exception CrossrefException

-- | "Network.HTTP.Client.HttpException" doesn't have an @Eq@ instance, so we
-- just don't check it, and assume that any two @CRHttpException@s with the same
-- DOIs are equal.
instance Eq CrossrefException where
  (==) :: CrossrefException -> CrossrefException -> Bool
  CRHttpException doi1 _  == CRHttpException doi2 _  = doi1 == doi2
  CRJsonException doi1 t1 == CRJsonException doi2 t2 = doi1 == doi2 && t1 == t2
  CRUnknownWorkException doi1 w1 == CRUnknownWorkException doi2 w2 =
    doi1 == doi2 && w1 == w2
  CROtherException doi1 t1 == CROtherException doi2 t2 =
    doi1 == doi2 && t1 == t2
  _ == _ = False

getDoiFromException :: CrossrefException -> DOI
getDoiFromException (CRHttpException        d _) = d
getDoiFromException (CRJsonException        d _) = d
getDoiFromException (CRUnknownWorkException d _) = d
getDoiFromException (CROtherException       d _) = d

-- | Step 1 is to fetch the raw JSON response from Crossref for a given DOI. Errors that can occur
-- here are CRHttpException (if the HTTPS request fails) or CRJSONInvalidException (if a JSON
-- response is obtained but it is entirely invalid). As long as the JSON response is valid, we move
-- on to the next stage.
getCrossrefJson
  ::
  -- | The http-client Manager. Note that this should be created using Network.Http.Client.TLS.tlsManagerSttings.
     NHC.Manager
  ->
  -- | Your email address. This is mandatory for making a polite request.
     Text
  ->
  -- | The DOI of interest.
     DOI
  ->
  -- | The raw JSON, represented as an Aeson Value.
     IO (Either CrossrefException Value)
getCrossrefJson manager email doi' = do
  let emailBS    = encodeUtf8 email
      escapedDoi = escapeURIString isUnescapedInURIComponent (T.unpack doi')
      uri        = "https://api.crossref.org/works/" <> escapedDoi
  request <- NHC.setQueryString [("mailto", Just emailBS)]
    <$> NHC.parseUrlThrow uri
  eitherExcResp :: Either NHC.HttpException (NHC.Response ByteString) <-
    CE.try $ NHC.httpLbs request manager
  -- ExceptT would make this cleaner, admittedly, but it's only two layers.
  pure $ case eitherExcResp of
    Left  exc  -> Left (CRHttpException doi' exc)
    Right resp -> case eitherDecode (NHC.responseBody resp) of
      Left  err -> Left (CRJsonException doi' (T.pack err))
      Right val -> Right val

-- | The same as 'getCrossrefJson', but does it from a file. You still need to
-- specify some kind of DOI for the error reporting: if you don't care about
-- that, then just pass an empty DOI (after all it is just a type alias for
-- Text).
getCrossrefJsonFile :: DOI -> FilePath -> IO (Either CrossrefException Value)
getCrossrefJsonFile doi' fname = do
  decoded <- eitherDecodeFileStrict fname
  case decoded of
       Left err -> pure $ Left (CRJsonException doi' (T.pack err))
       Right val -> pure $ Right val

-- | Step 2 is to get the 'message' component.
getJsonMessage
  :: DOI
  ->
  -- | The raw JSON from Crossref.
     Value
  -> Either CrossrefException Value
getJsonMessage doi' val = do
  let aesonParseResult =
        DAT.parseEither (withObject "Crossref JSON" (.: "message")) val
  first (CRJsonException doi' . T.pack) aesonParseResult

-- | Step 3a is to identify the type of work. This basically gives a CRUnknownWorkException if it's
-- not an article, which is technically a bug, but I don't know when I'll be able to work on adding
-- new types of works (e.g. books).
identifyWorkType :: DOI -> Value -> Either CrossrefException Text
identifyWorkType doi' messageVal = do
  let parsedWorkType =
        DAT.parseEither (withObject "Crossref response" (.: "type")) messageVal
  case parsedWorkType of
    Left failedParseMessage ->
      Left (CRJsonException doi' (T.pack failedParseMessage))
    Right wType -> case wType of
      "journal-article" -> Right wType
      "book"            -> Right wType
      _                 -> Left (CRUnknownWorkException doi' wType)

-- | Step 3b is to parse the 'message' component into the Abbotsbury data types, depending on which type
-- of work it is. Right now only Articles are supported.
parseCrossrefMessage
  :: DOI
  ->
  -- | @True@ to use internal list of journal abbreviations and fall back on
  -- Crossref, or @False@ just take Crossref data at face value.
     Bool
  ->
  -- | The 'message' component of the Crossref JSON data.
     Value
  -> Either CrossrefException Work -- Either an error message or the Work.
parseCrossrefMessage doi' useInternalAbbrevs messageVal = do
  parsedWorkType <- first
    (CRJsonException doi' . T.pack)
    (DAT.parseEither (withObject "Crossref response" (.: "type")) messageVal)
  -- Take an Aeson Parser, run it on a value (the Crossref message), and wrap up
  -- the Left String in a CrossrefException if it fails.
  let runParser
        :: (Object -> DAT.Parser a) -> Value -> Either CrossrefException a
      runParser parser obj = first
        (CRJsonException doi' . T.pack)
        (DAT.parseEither (withObject "Crossref response" parser) obj)
  -- Then it's as simple as fmapping the Work constructor over a call to
  -- runParser.
  case parsedWorkType of
    "journal-article" ->
      ArticleWork <$> runParser (parseArticle useInternalAbbrevs) messageVal
    "book" -> BookWork <$> runParser parseBook messageVal
    _      -> Left (CRUnknownWorkException doi' parsedWorkType)

-- | The parser which parses Articles.
parseArticle :: Bool -> Object -> DAT.Parser Article
parseArticle useInternalAbbrevs messageObj = do
  -- Title
  _articleTitle <- normalize NFC
    <$> safeHead "could not get title" (messageObj .: "title")
  -- Authors
  _authorArray    <- messageObj .: "author"
  _articleAuthors <- do
    auths <- mapM parsePerson _authorArray
    case NE.nonEmpty auths of
      Just x  -> pure x
      Nothing -> fail "expected at least one author, found none"
  -- Journal names
  _articleJournalLong <-
    safeHead "could not get journal title from container-title key"
    $  messageObj
    .: "container-title"
  -- Take the abbreviation from the Map if possible. Otherwise fall back on
  -- Crossref short-container-title, and if that isn't present, fall back on
  -- Crossref container-title.
  _articleJournalShort <-
    case (useInternalAbbrevs, getJournalAbbrev _articleJournalLong) of
      (True, Just short) -> pure short
      _                  -> safeHead "" (messageObj .: "short-container-title")
        <|> pure _articleJournalLong
  -- Year (prefer print publish date over online publish date as the former is
  -- the one usually used)
  publishedObj <-
    messageObj .: "published-print" <|> messageObj .: "published-online"
  _articleYear <- safeHead "year not found in date-parts"
    $ safeHead "date-parts was empty" (publishedObj .: "date-parts")
  -- Volume, issue, DOI
  _articleVolume <- messageObj .:? "volume" .!= ""
  _articleIssue  <- (messageObj .: "journal-issue" >>= (.: "issue")) <|> pure ""
  _articleDoi    <- messageObj .:? "DOI" .!= ""
  -- The pages are the most annoying one, I think. Try to get pages first, then
  -- fall back on article number, and if none of them exist, just use an empty
  -- page range.
  _articlePages  <-
    (PageRange <$> messageObj .: "page")
    <|> (ArticleNumber <$> messageObj .: "article-number")
    <|> pure (PageRange "")
  pure $ Article { .. }

parseBook :: Object -> DAT.Parser Book
parseBook messageObj = do
  _bookTitle        <- safeHead "could not get title" $ messageObj .: "title"
  -- Publisher and their location
  _bookPublisher    <- messageObj .: "publisher"
  _bookPublisherLoc <- messageObj .:? "publisher-location" .!= ""
  -- Authors and editors
  _bookAuthors      <- messageObj .: "author" >>= mapM parsePerson
  _bookEditors      <- messageObj .: "editor" >>= mapM parsePerson
  -- Year (prefer print publish date over online publish date as the former is the one usually used)
  publishedObj      <-
    messageObj .: "published-print" <|> messageObj .: "published-online"
  _bookYear <- safeHead "year not found in date-parts"
    $ safeHead "date-parts was empty" (publishedObj .: "date-parts")
  _bookEdition <- messageObj .:? "edition" .!= ""
  _bookIsbn    <- safeHead "ISBN was empty" (messageObj .: "ISBN") <|> pure ""
  pure $ Book { .. }

safeHead
  ::
  -- |  An error message to display if the list was empty.
     String
  ->
  -- | An Aeson parser which returns a list of results.
     DAT.Parser [a]
  ->
  -- | An Aeson parser which returns the first item.
     DAT.Parser a
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
parsePerson
  :: Object  -- ^ The Crossref JSON for an author.
  -> DAT.Parser Person
parsePerson personObj = do
  _given  <- fmap (normalize NFC . separateInitials) <$> (personObj .:? "given")
  _family <- normalize NFC <$> personObj .: "family"
  _suffix <- fmap (normalize NFC) <$> personObj .:? "suffix"
  pure Person { .. }

-- | This function enforces the separation of initials by a space, as described in parsePerson.
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

-- brittany-disable-next-binding

-- | A predefined list of @(full journal name, abbreviated journal name)@ which
-- can be used as an input to 'fixJournalShort'.
getJournalAbbrev :: Text -> Maybe Text
getJournalAbbrev long = M.lookup long abbrevMap
  where
    abbrevMap = M.fromList
      [ ("Angewandte Chemie International Edition"              , "Angew. Chem. Int. Ed."              )
      , ("Annual Reports on NMR Spectroscopy"                   , "Annu. Rep. NMR Spectrosc."          )
      , ("Biochemistry Journal"                                 , "Biochem. J."                        )
      , ("Chemical Physics Letters"                             , "Chem. Phys. Lett."                  )
      , ("Communications Chemistry"                             , "Commun. Chem."                      )
      , ("Journal of Biomolecular NMR"                          , "J. Biomol. NMR"                     )
      , ("Journal of Chemical Informatics and Modeling"         , "J. Chem. Inf. Model."               )
      , ("Journal of Computational Chemistry"                   , "J. Comp. Chem."                     )
      , ("Journal of Magnetic Resonance (1969)"                 , "J. Magn. Reson."                    )
      , ("Journal of Magnetic Resonance"                        , "J. Magn. Reson."                    )
      , ("Journal of Magnetic Resonance, Series A"              , "J. Magn. Reson., Ser. A"            )
      , ("Journal of Magnetic Resonance, Series B"              , "J. Magn. Reson., Ser. B"            )
      , ("Journal of Molecular Biology"                         , "J. Mol. Biol."                      )
      , ("Magnetic Resonance in Chemistry"                      , "Magn. Reson. Chem."                 )
      , ("Nature Chemistry"                                     , "Nat. Chem."                         )
      , ("Nature Communications"                                , "Nat. Commun."                       )
      , ("Nature Reviews Chemistry"                             , "Nat. Rev. Chem."                    )
      , ("Nature Reviews Methods Primers"                       , "Nat. Rev. Methods Primers"          )
      , ("Nucleic Acids Research"                               , "Nucleic Acids Res."                 )
      , ("Proceedings of the National Academy of Sciences"      , "Proc. Natl. Acad. Sci. U. S. A."    )
      , ("Progress in Nuclear Magnetic Resonance Spectroscopy"  , "Prog. Nucl. Magn. Reson. Spectrosc.")
      , ("Scientific Reports"                                   , "Sci. Rep."                          )
      , ("The Journal of Chemical Physics"                      , "J. Chem. Phys."                     )
      ]
