module Commands.Fetch
  ( runFetch
  ) where

import           Commands.Shared
import           Control.Concurrent.Async       ( forConcurrently )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString.Char8         as B
import           Data.Either                    ( partitionEithers )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Monoid                    ( First(..)
                                                , getFirst
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as TIO
import           Internal.Monad
import           Internal.Path
import           Lens.Micro.Platform
import qualified Network.HTTP.Client           as NHC
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types.Header
import           Reference
import           Replace.Megaparsec             ( breakCap )
import           System.Directory               ( doesFileExist )
import           Text.Megaparsec
import           Text.Megaparsec.Char

prefix :: Text
prefix = "fetch: "

runFetch :: Args -> CmdInput -> CmdOutput
runFetch args input = do
  let refs = refsin input
      cwd  = cwdin input
  -- If no refs present, error immediately
  errorOnNoRefs prefix input
  -- Parse arguments
  refnos <- parseInCommand pRefnos args prefix
  let argsRefnos = resolveRefnosWith refs refnos
  -- Figure out which refnos to fetch (but this is only preliminary, because we
  -- still need to remove refs which don't need to be fetched)
  initialRefnosAndRefs <- getActiveRefs prefix argsRefnos False input
  -- Filter out anything that shouldn't be fetched
  let getRnoDoiPairs :: (Int, Reference) -> ExceptT Text IO (Int, DOI)
      getRnoDoiPairs (rno, ref) = do
        let w        = ref ^. work
        let maybeDoi = w ^? _article . doi
        -- Check if full text is already present
        fullTextExists <- liftIO $ doesFileExist $ getPdfPath FullText cwd w
        when
          fullTextExists
          (throwError
            (prefix <> refnoT rno <> "full text already found in library")
          )
        -- Check if the work has a DOI (books don't)
        case maybeDoi of
          Nothing ->
            throwError (prefix <> refnoT rno <> "DOI is not available")
          Just doi' -> pure (rno, doi')
  (errors, refnosAndDois) <-
    liftIO
    $   partitionEithers
    <$> mapM (runExceptT . getRnoDoiPairs) initialRefnosAndRefs
  -- Print errors as necessary
  forM_ errors printError
  -- Real work starts here.
  let (refnosToFetch, doisToFetch) = unzip refnosAndDois
  when (null refnosToFetch) (throwError $ prefix <> "no references to fetch")
  email         <- getUserEmail prefix
  manager       <- liftIO $ NHC.newManager tlsManagerSettings
  refnoOutcomes <- liftIO $ forConcurrently refnosAndDois $ \(rno, doi) -> do
    -- Get the URL first.
    maybeUrl <- getFullTextUrl email manager doi
    case maybeUrl of
      Nothing -> do
        printError $ prefix <> refnoT rno <> "could not get URL for full text"
        pure Nothing
      -- If we managed to get it, then download the PDF.
      Just url -> do
        let destination = getPdfPath FullText cwd (refs IM.! rno)
        TIO.putStrLn $ "downloading PDF for DOI " <> doi <> "..."
        success <- downloadPdf email (Just manager) url destination
        unless success
               (printError $ prefix <> refnoT rno <> "PDF download failed")
        pure $ Just rno
  -- We return the refnos that succeeded.
  pure $ SCmdOutput refs (IS.fromList <$> sequence refnoOutcomes)

data Publisher = ACS | Nature
               | Science | Springer
               | TaylorFrancis | Wiley
               | AnnRev | RSC | Elsevier
               deriving (Eq, Show, Ord)
type Identifier = Text

-- | Get the URL to the full text PDF. Returns @Just url@ if it succeeds and
-- @Nothing@ if it fails.
getFullTextUrl :: Text -> NHC.Manager -> DOI -> IO (Maybe Text)
getFullTextUrl email manager doi = do
  TIO.putStrLn $ "getting PDF link for DOI " <> doi <> "..."
  let doi_url = T.unpack $ "https://doi.org/" <> doi
  -- Check the website to see what headers it returns us.
  req <- politeReq email <$> NHC.parseUrlThrow doi_url
  maybePubl <- NHC.withResponse req manager (getPublFromResponse doi)
  pure $ uncurry publisherToUrl <$> maybePubl

-- | Identify the publisher and the identifier from a HTML response. The @doi@
-- parameter is passed in to help construct the identifier (for some publishers,
-- the identifier is the DOI itself, or something closely related to it).
-- The IO in the type signature is needed because at this point we haven't read
-- in the response body yet. (We /could/ do so before this, but that would
-- necessitate holding the entire response in memory.)
--
-- Implementation-wise, this tries to first get it from the headers (using
-- 'getPublFromHeaders'); then it tries to get it by parsing the body (using
-- 'getPublFromBodyLines').
getPublFromResponse
  :: DOI -> NHC.Response NHC.BodyReader -> IO (Maybe (Publisher, Identifier))
getPublFromResponse doi resp = do
    -- Try to get the information from the headers first.
  case getPublFromHeaders doi (NHC.responseHeaders resp) of
    Just (p, i) -> pure $ Just (p, i)
    Nothing     -> do
       -- If that failed, then we'll have to use the body. And this is going
       -- to be a bit ugly, because I can't wrap my head around http-conduit
       -- at the moment.
      let getPubl2 decodeContinuation leftover = do
            -- Read in a ByteString. This can be cut in the middle of a UTF-8
            -- character, and also can be cut in the middle of a line... We use
            -- Data.Text.Encoding.streamDecodeUtf8 to handle the first case; and
            -- we also manually keep track of line breaks to handle the second
            -- case. Note that this requires we use T.splitOn "\n" instead of
            -- T.lines, because the latter misbehaves if the string ends with a
            -- newline.
            bs <- NHC.brRead $ NHC.responseBody resp
            if B.null bs && T.null leftover
              then pure Nothing     -- body ended
              else do
                let (T.Some text _ cont') = decodeContinuation bs
                    (ts, leftover') = case T.splitOn "\n" (leftover <> text) of
                      []  -> ([], "")
                      [x] -> ([x], "")
                      xs  -> (init xs, last xs)
                case getFirst $ foldMap (First . getPublFromBodyLine doi) ts of
                  Just (p', i') -> pure $ Just (p', i')
                  Nothing       -> getPubl2 cont' leftover'
      getPubl2 T.streamDecodeUtf8 ""

-- | Attempt to detect the (publisher, identifier) combination from the HTTP
-- headers alone. This means we don't have to parse the body.
getPublFromHeaders :: DOI -> ResponseHeaders -> Maybe (Publisher, Identifier)
getPublFromHeaders doi headers
  | isInHeaders "pubs.acs.org" "Content-Security-Policy" headers = Just
    (ACS, doi)
  | isInHeaders "www.nature.com" "X-Forwarded-Host" headers = Just
    (Nature, T.tail . snd . T.breakOn "/" $ doi)
  | isInHeaders "science.sciencemag.org" "Link" headers = Just
    ( Science
    , snd . T.breakOnEnd "/content/" . fst . T.breakOn ">" $ getFirstHeaderValue
      "Link"
      headers
    )
  | isInHeaders ".springer.com" "Set-Cookie" headers = Just (Springer, doi)
  | otherwise = Nothing

-- | Attempt to detect the (publisher, identifier) combination from the HTTP
-- body. This is rather more involved, and basically involves running a bunch of
-- Megaparsec parsers (which replace the regexes in the Python version) on each
-- line of the HTML response body.
getPublFromBodyLine :: DOI -> Text -> Maybe (Publisher, Identifier)
getPublFromBodyLine doi = breakCap' pPubl
 where
  -- Version of breakCap that throws away the prefix and suffix. Type is a bit
  -- specialised, but not hugely important here.
  breakCap' :: Parser a -> Text -> Maybe a
  breakCap' parser input = (\(_, m, _) -> m) <$> breakCap parser input
  -- Bunch of helper functions.
  isQuote :: Char -> Bool
  isQuote c = c == '\'' || c == '\"'
  quote :: Parser Char
  quote = satisfy isQuote
  notQuote :: Parser Text
  notQuote = takeWhileP Nothing (not . isQuote)
  inQuotes :: Parser Text -> Parser Text
  inQuotes p = quote *> p <* quote
  -- The actual parsers
  pPubl, pRsc, pWiley, pElsevier, pAnnRev, pTaylorFrancis
    :: Parser (Publisher, Identifier)
  pPubl = choice $ map try [pRsc, pWiley, pElsevier, pAnnRev, pTaylorFrancis]
  pRsc  = do
    string "<meta content="
    iden <- inQuotes
      (string "https://pubs.rsc.org/en/content/articlepdf/" *> notQuote)
    space1
    string "name="
    inQuotes (string "citation_pdf_url")
    space
    string "/>"
    pure (RSC, iden)
  pWiley = do
    string "<meta name="
    inQuotes (string "citation_publisher")
    space1
    string "content="
    publisherName <- inQuotes notQuote
    guard $ "John Wiley" `T.isInfixOf` publisherName
    space
    optional $ char '/'
    char '>'
    pure (Wiley, doi)
  pElsevier = do
    string "<input type="
    inQuotes (string "hidden")
    space1
    string "name="
    inQuotes (string "redirectURL")
    space1
    string "value="
    idenWithSuffix <- inQuotes
      (string "https%3A%2F%2Fwww.sciencedirect.com%2Fscience%2Farticle%2Fpii%2F"
      *> notQuote
      )
    let maybeIden = T.stripSuffix "%3Fvia%253Dihub" idenWithSuffix
    space1
    string "id="
    inQuotes (string "redirectURL")
    string "/>"
    case maybeIden of
      Just iden -> pure (Elsevier, iden)
      Nothing   -> fail ""
  pAnnRev = do
    string "<meta name="
    inQuotes (string "dc.Publisher")
    space1
    string "content="
    publisherName <- inQuotes notQuote
    guard $ "Annual Reviews" `T.isInfixOf` publisherName
    space
    optional $ char '/'
    char '>'
    pure (AnnRev, doi)
  pTaylorFrancis = do
    string "<meta name="
    inQuotes (string "dc.Publisher")
    space1
    string "content="
    publisherName <- inQuotes notQuote
    guard $ "Taylor" `T.isInfixOf` publisherName
    space
    optional $ char '/'
    char '>'
    pure (TaylorFrancis, doi)

-- | We assume (and this is borne out in practice) that the URL of the full text
-- PDF can be uniquely determined given knowledge of: (a) the publisher; and (b)
-- some kind of identifier, which may be the DOI, or something else entirely.
-- This function provides the rules for constructing this URL.
publisherToUrl :: Publisher -> Identifier -> Text
publisherToUrl p iden
  | p == ACS
  = "https://pubs.acs.org/doi/pdf/" <> iden
  | p == Wiley
  = "https://onlinelibrary.wiley.com/doi/pdfdirect/" <> iden
  | p == Elsevier
  = "https://www.sciencedirect.com/science/article/pii/" <> iden <> "/pdfft"
  | p == Nature
  = "https://www.nature.com/articles/" <> iden <> ".pdf"
  | p == Science
  = "https://science.sciencemag.org/content/sci/" <> iden <> ".full.pdf"
  | p == Springer
  = "https://link.springer.com/content/pdf/" <> iden <> ".pdf"
  | p == TaylorFrancis
  = "https://www.tandfonline.com/doi/pdf/" <> iden
  | p == AnnRev
  = "https://www.annualreviews.org/doi/pdf/" <> iden
  | p == RSC
  = "https://pubs.rsc.org/en/content/articlepdf/" <> iden
