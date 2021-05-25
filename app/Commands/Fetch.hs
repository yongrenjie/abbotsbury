{-# LANGUAGE MultiWayIf #-}

module Commands.Fetch
  ( runFetch
  ) where

import           Commands.Shared
import           Data.ByteString.UTF8           ( fromString )
import qualified Data.CaseInsensitive          as CI
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Data.Text.IO                  as TIO
-- import           Data.Version                   ( showVersion )
import           Internal.Monad
import           Lens.Micro.Platform
import qualified Network.HTTP.Client           as NHC
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Types.Header
-- import           Paths_abbotsbury               ( version )
import           Reference

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
  -- Figure out which refnos to print
  refnosToFetch <- getActiveRefnos prefix argsRefnos input
  let doisToFetch =
        (refs `IM.restrictKeys` refnosToFetch) ^.. each . work . _article . doi
  -- Error out if there's nothing to do (e.g. if a book was selected)
  when (null doisToFetch) $ throwError (prefix <> "no works to fetch")
  -- Real work starts here.
  eitherEitherUrls <- liftIO
    $ mapM (runExceptT . runExceptT . getFullTextUrl) doisToFetch
  let refnosAndeEUrls = zip (IS.toList refnosToFetch) eitherEitherUrls
  -- Filter only the successful results, which have the form Right (Left (P, I))
  let refnosAndUrls = do
        (rno, x) <- refnosAndeEUrls
        case x of
          Right (Left url) -> pure (rno, url)
          _                -> []
  liftIO $ print refnosAndUrls
  throwError (prefix <> "not implemented yet")

data Publisher = ACS | Nature
               | Science | Springer
               | TaylorFrancis | Wiley
               | AnnRev | RSC | Elsevier
               deriving (Eq, Show, Ord)
type Identifier = Text
type ErrorText = Text
type FullTextUrl = Text

-- | The type signature of this is a bit odd.
-- The outer layer of ExceptT is for short-circuiting from the function /with a
-- successful result/, i.e. having identified the correct publisher and
-- identifier.
-- The inner layer of ExceptT is for reporting errors.
-- Note that because monad transformers work "inside out"... the return result
-- of @Left X@ indicates an error, whereas a successful early return corresponds
-- to @Right (Left (P, I))@.
getFullTextUrl :: DOI -> ExceptT FullTextUrl (ExceptT ErrorText IO) ()
getFullTextUrl doi = do
  let fail :: ErrorText -> ExceptT FullTextUrl (ExceptT ErrorText IO) ()
      fail = lift . throwError
  let earlyReturn
        :: Publisher
        -> Identifier
        -> ExceptT FullTextUrl (ExceptT ErrorText IO) ()
      earlyReturn p i = throwError (publisherToUrl p i)
  let doi_url = T.unpack $ "https://doi.org/" <> doi
  -- Check the website to see what headers it returns us.
  email          <- lift $ encodeUtf8 <$> getUserEmail prefix
  manager        <- liftIO $ NHC.newManager tlsManagerSettings
  initialRequest <- NHC.parseUrlThrow doi_url
  -- Unfortunately, Springer refuses to return proper information if I use the
  -- "true" user-agent header, so I have to feed it something which looks like a
  -- web browser.
  -- let userAgent = fromString $ "abbotsbury/" <> showVersion version
  let userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36"
  let request = initialRequest
        { NHC.requestHeaders = [("mailto", email), ("user-agent", userAgent)]
        }
  response <- liftIO $ NHC.httpNoBody request manager
  let headers = NHC.responseHeaders response
  if
    | isInHeaders "pubs.acs.org" "Content-Security-Policy" headers
    -> earlyReturn ACS doi
    | isInHeaders "www.nature.com" "X-Forwarded-Host" headers
    -> earlyReturn Nature (T.tail . snd . T.breakOn "/" $ doi)
    | isInHeaders "science.sciencemag.org" "Link" headers
    -> do
      let link = getFirstHeaderValue "Link" headers
      earlyReturn
        Science
        (snd . T.breakOnEnd "/content/" . fst . T.breakOn ">" $ link)
    | isInHeaders ".springer.com" "Set-Cookie" headers
    -> earlyReturn Springer doi
    | otherwise
    -> do
      liftIO $ print $ NHC.responseHeaders response  -- for debugging purposes
      -- should start to read and parse the body here.
  -- Bugs: tandf doesn't work. It refuses to give me proper headers. It's kind
  -- of an edge case, to be honest...
    

publisherToUrl :: Publisher -> Identifier -> FullTextUrl
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


isInHeaders :: Text -> Text -> ResponseHeaders -> Bool
isInHeaders value headerName = any (T.isInfixOf value . decodeUtf8 . snd)
  . filter ((== CI.mk (encodeUtf8 headerName)) . fst)


-- | Returns an empty Text if the header is not found.
getFirstHeaderValue :: Text -> ResponseHeaders -> Text
getFirstHeaderValue headerName hdrs =
  case filter ((== CI.mk (encodeUtf8 headerName)) . fst) hdrs of
    []      -> ""
    (h : _) -> decodeUtf8 . snd $ h
