module CovenantEyes.Internal.UrlEncoding where

import BasePrelude
import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.HTTP.Types.URI as Uri

import CovenantEyes.Types (CeUser(..))


class UrlEncodable a where
  urlEncode :: a -> ByteString
  urlDecode :: ByteString -> a

instance UrlEncodable ByteString where
  urlEncode = Uri.urlEncode False
  urlDecode = Uri.urlDecode False

instance UrlEncodable Text where
  urlEncode = urlEncode . encodeUtf8
  urlDecode = decodeUtf8 . urlDecode

instance (CI.FoldCase a, UrlEncodable a) => UrlEncodable (CI a) where
  urlEncode = urlEncode . CI.original
  urlDecode = CI.mk . urlDecode

instance UrlEncodable CeUser where
  urlEncode = urlEncode . getCeUsername
  urlDecode = CeUser . urlDecode
