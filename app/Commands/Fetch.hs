{-# LANGUAGE MultiWayIf #-}

module Commands.Fetch
  ( runFetch
  ) where

import Data.List (intersperse)

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
import           Data.Text.Encoding             ( Decoding(..)
                                                , decodeUtf8
                                                , encodeUtf8
                                                , streamDecodeUtf8
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
import           System.IO                      ( Handle(..)
                                                , IOMode(..)
                                                , withBinaryFile
                                                )
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
        -- Check if full text is already present
        fullTextExists <- liftIO $ doesFileExist $ getPDFPath FullText cwd w
        when
          fullTextExists
          (throwError
            (  prefix
            <> "refno "
            <> T.pack (show rno)
            <> ": full text already found in library"
            )
          )
        -- Check if the work has a DOI (books don't)
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
  -- Real work starts here.
  let (refnosToFetch, doisToFetch) = unzip refnosAndDois
      refsToFetch                  = [ refs IM.! rno | rno <- refnosToFetch ]
  when (null refsToFetch) (throwError $ prefix <> "no references to fetch")
  email      <- getUserEmail prefix
  manager    <- liftIO $ NHC.newManager tlsManagerSettings
  -- TODO: parallelise?? I think mapM is sequential...
  eitherUrls <- liftIO
    $ mapM (runExceptT . getFullTextUrl email manager) doisToFetch
  -- Print errors for the unsuccessful results
  let failedRefnos = [ rno | (rno, Left err) <- zip refnosToFetch eitherUrls ]
  forM_
    failedRefnos
    (\rno ->
      printError
        $  prefix
        <> "refno "
        <> T.pack (show rno)
        <> ": could not get URL for full text"
    )
  -- Filter only the successful results
  let refsAndUrls =
        [ (ref, url) | (ref, Right url) <- zip refsToFetch eitherUrls ]
  mapM_ (downloadPdf cwd email manager) refsAndUrls
  pure $ SCmdOutput refs (Just $ IS.fromList refnosToFetch)

data Publisher = ACS | Nature
               | Science | Springer
               | TaylorFrancis | Wiley
               | AnnRev | RSC | Elsevier
               deriving (Eq, Show, Ord)
type Identifier = Text

-- | Get the URL to the full text PDF.
getFullTextUrl :: Text -> NHC.Manager -> DOI -> ExceptT Text IO Text
getFullTextUrl email manager doi = do
  liftIO $ TIO.putStrLn $ "getting PDF link for DOI " <> doi <> "..."
  let doi_url = T.unpack $ "https://doi.org/" <> doi
  -- Check the website to see what headers it returns us.
  req <- politeReq email <$> NHC.parseUrlThrow doi_url
  maybePubl <- liftIO $ NHC.withResponse req manager $ \resp -> do
    -- Try to get the information from the headers first.
    case getPublFromHeaders doi (NHC.responseHeaders resp) of
      Just (p, i) -> pure $ Just (p, i)
      Nothing     -> do
         -- If that failed, then we'll have to use the body. And this is going
         -- to be a bit ugly, because I can't wrap my head around http-conduit
         -- at the moment.
        let
          getPubl2 decodeContinuation leftover = do
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
                let (Some text _ cont') = decodeContinuation bs
                    (ts, leftover') = case T.splitOn "\n" (leftover <> text) of
                      []  -> ([], "")
                      [x] -> ([x], "")
                      xs  -> (init xs, last xs)
                -- mapM_ TIO.putStrLn (intersperse "------------" ts)
                case getFirst $ foldMap (First . getPublFromBody doi) ts of
                  Just (p', i') -> pure $ Just (p', i')
                  Nothing       -> getPubl2 cont' leftover'
        getPubl2 streamDecodeUtf8 ""
  case maybePubl of
    Just (p, i) -> pure $ publisherToUrl p i
    Nothing     -> throwError ""

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
getPublFromBody :: DOI -> Text -> Maybe (Publisher, Identifier)
getPublFromBody doi = breakCap' pPubl
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

-- | Download a PDF.
downloadPdf
  :: FilePath -> Text -> NHC.Manager -> (Reference, Text) -> ExceptT Text IO ()
downloadPdf cwd email manager (ref, url) = do
  liftIO
    $  TIO.putStrLn
    $  "downloading PDF for DOI "
    <> ref
    ^. work
    .  _article
    .  doi
    <> "..."
  let destination = getPDFPath FullText cwd ref
  req       <- politeReq email <$> NHC.parseUrlThrow (T.unpack url)
  succeeded <- liftIO $ NHC.withResponse req manager $ \resp -> do
    let hdrs = NHC.responseHeaders resp
        body = NHC.responseBody resp
    if
      | isInHeaders "application/pdf" "content-type" hdrs
      -> withBinaryFile destination WriteMode (normalDownloadPdf body)
      | "sciencedirect"
        `T.isInfixOf` url
        &&            isInHeaders "text/html" "content-type" hdrs
      -> withBinaryFile destination WriteMode (elsevierDownloadPdf body)
      | otherwise
      -> pure False
  if succeeded
    then pure ()
    else printError $ prefix <> "PDF download failed for link " <> url
 where
  normalDownloadPdf :: NHC.BodyReader -> Handle -> IO Bool
  normalDownloadPdf body hdl = do
    let loop = do
          bs <- NHC.brRead body
          if B.null bs then pure () else B.hPut hdl bs >> loop
    loop
    pure True
  -- Elsevier's "PDF URL" is not really a PDF, but rather a HTML page which
  -- redirect us to a PDF. This wouldn't be problematic if they would just use
  -- ordinary HTTP redirects; however, for whatever reason, they redirect us
  -- with /JavaScript/ which means we need to parse the body.
  elsevierDownloadPdf :: NHC.BodyReader -> Handle -> IO Bool
  elsevierDownloadPdf body hdl = do
    -- It's quite a small page, so just read the whole thing in.
    text <- decodeUtf8 . B.concat <$> NHC.brConsume body
    let
      ws =
        filter ("window.location" `T.isPrefixOf`) . map T.strip . T.lines $ text
    case ws of
      []      -> pure False
      (x : _) -> do
        case T.splitOn "'" x of
          [_, link, _] -> do
            TIO.putStrLn "redirected by Elsevier..."
            req' <- politeReq email <$> NHC.parseUrlThrow (T.unpack link)
            NHC.withResponse req' manager $ \resp' -> do
              let body' = NHC.responseBody resp'
              normalDownloadPdf body' hdl
          _ -> pure False

-- | Add some courtesy headers to a request.
--
-- Unfortunately, Springer refuses to return proper information if I use the
-- "true" user-agent header, so I have to feed it something which looks like a
-- web browser. However, even with this spoofed user-agent, T&F papers don't
-- work. It refuses to give me proper headers. To be fair, T&F is kind of an
-- edge case...
politeReq :: Text -> NHC.Request -> NHC.Request
politeReq email r = r
  { NHC.requestHeaders = [ ("mailto"    , encodeUtf8 email)
                         , ("user-agent", userAgent)
                         ]
  }
 where
  userAgent :: B.ByteString
  userAgent =
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"
      <> " AppleWebKit/537.36 (KHTML, like Gecko)"
      <> " Chrome/90.0.4430.212 Safari/537.36"
