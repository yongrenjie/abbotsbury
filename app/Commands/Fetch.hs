{-# LANGUAGE MultiWayIf #-}

module Commands.Fetch
  ( runFetch
  ) where

import           Commands.Shared
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.ByteString.UTF8           ( fromString )
import qualified Data.CaseInsensitive          as CI
import           Data.Either                    ( partitionEithers )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Monoid                    ( First(..)
                                                , getFirst
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
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

-- import           Data.Version                   ( showVersion )
-- import           Paths_abbotsbury               ( version )

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
  specifiedRefnos <- getActiveRefnos prefix argsRefnos input
  -- Filter out anything that shouldn't be fetched
  let getRnoDoiPairs :: Int -> ExceptT Text IO (Int, DOI)
      getRnoDoiPairs rno = do
        let w        = refs ^?! ix rno . work
        let maybeDoi = w ^? _article . doi
        fullTextExists <- liftIO $ doesFileExist $ getPDFPath FullText cwd w
        -- Error out fi
        when
          fullTextExists
          (throwError
            (  prefix
            <> "refno "
            <> T.pack (show rno)
            <> ": full text already found in library"
            )
          )
        case maybeDoi of
          Nothing ->
            throwError
              (  prefix
              <> "refno "
              <> T.pack (show rno)
              <> ": DOI is not available"
              )
          Just doi' -> pure (rno, doi')
  (errors, refnosAndDois) <- liftIO $ partitionEithers <$> mapM
    (runExceptT . getRnoDoiPairs)
    (IS.toList specifiedRefnos)
  -- Print errors as necessary
  forM_ errors printError
  -- Filter out anything that already has a full text
  let (refnosToFetch, doisToFetch) = unzip refnosAndDois
  -- Real work starts here.
  email      <- getUserEmail prefix
  eitherUrls <- liftIO $ mapM (runExceptT . getFullTextUrl email) doisToFetch
  let refnosAndEUrls = zip refnosToFetch eitherUrls
  -- Filter only the successful results
  let refnosAndUrls  = [ (rno, url) | (rno, Right url) <- refnosAndEUrls ]
  liftIO $ print refnosAndUrls
  throwError (prefix <> "not finished implementing yet")

data Publisher = ACS | Nature
               | Science | Springer
               | TaylorFrancis | Wiley
               | AnnRev | RSC | Elsevier
               deriving (Eq, Show, Ord)
type Identifier = Text

-- | Get the URL to the full text PDF.
--
-- Unfortunately, Springer refuses to return proper information if I use the
-- "true" user-agent header, so I have to feed it something which looks like a
-- web browser. However, even with this spoofed user-agent, T&F papers don't
-- work. It refuses to give me proper headers. To be fair, T&F is kind of an
-- edge case...
getFullTextUrl :: Text -> DOI -> ExceptT Text IO Text
getFullTextUrl email doi = do
  let doi_url = T.unpack $ "https://doi.org/" <> doi
  -- Check the website to see what headers it returns us.
  manager        <- liftIO $ NHC.newManager tlsManagerSettings
  initialRequest <- NHC.parseUrlThrow doi_url
  -- let userAgent = fromString $ "abbotsbury/" <> showVersion version
  let userAgent =
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"
          <> " AppleWebKit/537.36 (KHTML, like Gecko)"
          <> " Chrome/90.0.4430.212 Safari/537.36"
  let request = initialRequest
        { NHC.requestHeaders = [ ("mailto"    , encodeUtf8 email)
                               , ("user-agent", userAgent)
                               ]
        }
  response <- liftIO $ NHC.httpLbs request manager
  let headers   = NHC.responseHeaders response
      lbsToText = decodeUtf8 . B.concat . LB.toChunks
      bodyLines = T.lines . lbsToText $ NHC.responseBody response
  -- we'll get rid of these variables later; for now it's just debugging
  let x = getPublFromHeaders doi headers
      y = getPublFromBody doi bodyLines
  liftIO $ print x
  liftIO $ print y
  case x <|> y of
    Just (p, i) -> pure $ publisherToUrl p i
    Nothing     -> throwError "full text not found"

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
-- body. This is rather more involved. The input to this function must be a list
-- of lines of Text, which have already been decoded.
getPublFromBody :: DOI -> [Text] -> Maybe (Publisher, Identifier)
getPublFromBody doi = getFirst . foldMap (First . breakCap' pPubl)
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

-- | @isInHeaders val name headers@ checks if:
--  (a) there is one or more headers with the given @name@;
--  (b) any of the values of these headers contains the text @val@ anywhere in
--  it.
isInHeaders :: Text -> Text -> ResponseHeaders -> Bool
isInHeaders value headerName = any (T.isInfixOf value . decodeUtf8 . snd)
  . filter ((== CI.mk (encodeUtf8 headerName)) . fst)

-- | Get the value of the first header with the specified name. Returns an empty
-- Text if the header is not found.
getFirstHeaderValue :: Text -> ResponseHeaders -> Text
getFirstHeaderValue headerName hdrs =
  case filter ((== CI.mk (encodeUtf8 headerName)) . fst) hdrs of
    []      -> ""
    (h : _) -> decodeUtf8 . snd $ h
