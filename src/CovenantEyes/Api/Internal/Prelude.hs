module CovenantEyes.Api.Internal.Prelude
  ( module X
  , Defaultable, throwing, orLeft, throwingLeft, throwingLeftAs
  ) where

import Prelude as X
import Control.Monad as X (unless, join, forM)
import Control.Monad.Catch as X
import Control.Monad.Except as X (ExceptT, runExceptT)
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Trans.Class as X (lift)
import Data.ByteString as X (ByteString)
import Data.Function as X ((&))
import Data.Maybe as X (fromMaybe, fromJust)
import Data.Monoid as X ((<>))
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)


class Defaultable f where
  defaultingTo :: a -> f a -> a

instance Defaultable Maybe where
  defaultingTo = fromMaybe

instance Defaultable (Either a) where
  defaultingTo _ (Right a) = a
  defaultingTo b (Left _)  = b

throwing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwing e = throwingLeft . orLeft e

orLeft :: l -> Maybe r -> Either l r
orLeft l = maybe (Left l) Right

throwingLeftAs :: (MonadThrow m, Exception e) => (l -> e) -> Either l r -> m r
throwingLeftAs lToExcept (Left l)  = throwM $ lToExcept l
throwingLeftAs _         (Right r) = return r

throwingLeft :: (MonadThrow m, Exception l) => Either l r -> m r
throwingLeft = throwingLeftAs id
