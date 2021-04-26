module Abbot.Crossref where


import Abbot.Work


import           Data.Aeson
import           Data.Text                      ( Text )
-- import qualified Data.Text                     as T
import           Network.HTTP.Req
-- import qualified Text.URI                      as URI


-- | Fetch the raw JSON response from Crossref for a given DOI.
-- TODO: How to encode possible failures?
getCrossrefJSON :: Text -- ^ Your email address. This is mandatory for making a polite request.
  -> DOI -- ^ The desired DOI.
  -> IO Value
getCrossrefJSON email doi' = runReq defaultHttpConfig $ do
  resp <- req GET (https "api.crossref.org" /: "works" /: doi') NoReqBody jsonResponse $
    "mailto" =: email
  pure (responseBody resp)


parseCrossrefJSON :: Value -> Work
parseCrossrefJSON = undefined
