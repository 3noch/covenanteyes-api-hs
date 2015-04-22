module CovenantEyes.Internal.Errors where

import BasePrelude
import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Data.Either (either)
import Data.Maybe  (fromMaybe, maybe)


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

leftAs :: (l -> r) -> Either l r -> r
leftAs lToR = either lToR id

throwingLeftAs :: (MonadThrow m, Exception e) => (l -> e) -> Either l r -> m r
throwingLeftAs lToExcept (Left l)  = throwM $ lToExcept l
throwingLeftAs _         (Right r) = return r

throwingLeft :: (MonadThrow m, Exception l) => Either l r -> m r
throwingLeft = throwingLeftAs id
