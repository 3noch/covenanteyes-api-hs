module CovenantEyes.Api.Internal.Http where

import           CovenantEyes.Api.Internal.Prelude

import           Data.Aeson (FromJSON)
import           Network.HTTP.Types.Header (hContentType)
import qualified Pipes.Aeson as PJson
import           Pipes.HTTP
import           Pipes.Parse (evalStateT)

import           CovenantEyes.Api.Types
import           CovenantEyes.Api.Internal.Errors

downloadJson :: FromJSON a => Manager -> Request -> EitherT SomeException IO a
downloadJson manager req = syncIO $ do
  withHTTP req manager $ \resp -> do
    let contentType = hContentType `lookup` responseHeaders resp
    unless (contentType == Just jsonContentType) $ throwM (UnexpectedContentType jsonContentType contentType)
    evalStateT PJson.decode (responseBody resp)
      >>= throwing NoData
      >>= throwingLeftAs DecodingError
  where
    jsonContentType = "application/json"


data HttpMethod = GET | PUT | POST deriving (Show, Eq)

applyBasicAuthCreds :: BasicAuthCreds -> Request -> Request
applyBasicAuthCreds (BasicAuthCreds user pass) = applyBasicAuth user pass

applyUserAgent :: ByteString -> Request -> Request
applyUserAgent userAgent req
  = req { requestHeaders = ("User-Agent", userAgent) : requestHeaders req }

applyContentType :: ByteString -> Request -> Request
applyContentType contentType req
  = req { requestHeaders = ("Content-Type", contentType) : requestHeaders req }

applyMethod :: HttpMethod -> Request -> Request
applyMethod GET req  = req { method = "GET" }
applyMethod PUT req  = req { method = "PUT" }
applyMethod POST req = req { method = "POST" }
