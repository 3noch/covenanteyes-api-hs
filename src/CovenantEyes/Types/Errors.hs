module CovenantEyes.Types.Errors where

import           BasePrelude
import           Control.Monad.Catch (Exception)
import           Data.ByteString (ByteString)
import qualified Pipes.Aeson as PJson

data CeApiException = NoData
                    | UnexpectedContent
                    | UnexpectedContentType ByteString (Maybe ByteString)
                    | DecodingError PJson.DecodingError
                    deriving (Eq, Show, Typeable)
instance Exception CeApiException
