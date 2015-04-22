module CovenantEyes.Types
  ( BasicAuthCreds(..)
  , CredsFor(..)
  , CeUser(..)
  , CeClient(..)
  , ApiRoot(..), mkApiRoot
  , Secure
  , NonSecure
  , CeApiException(..)
  ) where

import           BasePrelude
import           Control.Monad.Catch (Exception)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.CaseInsensitive (CI)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Pipes.Aeson as PJson

data BasicAuthCreds = BasicAuthCreds
  { basicAuthUser     :: ByteString
  , basicAuthPassword :: ByteString
  } deriving (Show, Eq, Ord)

data CredsFor a = CredsFor { credId         :: a
                           , basicAuthCreds :: BasicAuthCreds }

deriving instance Eq a   => Eq   (CredsFor a)
deriving instance Ord a  => Ord  (CredsFor a)
deriving instance Show a => Show (CredsFor a)


newtype CeUser   = CeUser   { getCeUsername   :: CI Text } deriving (Show, Eq, Ord)
newtype CeClient = CeClient { getCeClientName :: Text    } deriving (Show, Eq, Ord)

instance IsString CeUser where
  fromString = CeUser . fromString . fromString

instance IsString CeClient where
  fromString = CeClient . fromString

newtype ApiRoot a = ApiRoot { unApiRoot :: ByteString }
                    deriving (Show, Eq)

instance IsString (ApiRoot a) where
  fromString = ApiRoot . fromString

data Secure
data NonSecure

mkApiRoot :: ByteString -> ApiRoot a
mkApiRoot root = ApiRoot $ if B.last root == '/'
                  then B.init root
                  else root



data CeApiException = NoData
                    | UnexpectedContent
                    | UnexpectedContentType ByteString (Maybe ByteString)
                    | DecodingError PJson.DecodingError
                    deriving (Eq, Show, Typeable)
instance Exception CeApiException
