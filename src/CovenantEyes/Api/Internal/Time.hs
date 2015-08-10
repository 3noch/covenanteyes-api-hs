module CovenantEyes.Api.Internal.Time
  ( UTCTime, addUTCTime
  , Clk.TimeSpec
  , Nanosec(..), timeSpecAsNanosec, nanosecAsTimeSpec, addTimeOffset, nanoFactor
  ) where

import CovenantEyes.Api.Internal.Prelude
import Data.Time.Clock (UTCTime, addUTCTime)
import System.Clock as Clk

newtype Nanosec = Nanosec Integer deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

timeSpecAsNanosec :: Clk.TimeSpec -> Nanosec
timeSpecAsNanosec = Nanosec . Clk.timeSpecAsNanoSecs

nanosecAsTimeSpec :: Nanosec -> Clk.TimeSpec
nanosecAsTimeSpec (Nanosec count) = TimeSpec (fromIntegral sec) (fromIntegral nano)
  where (sec, nano) = count `quotRem` nanoFactor

addTimeOffset :: Nanosec -> UTCTime -> UTCTime
addTimeOffset (Nanosec nanosec) = addUTCTime (realToFrac nanosec / nanoFactor)

nanoFactor :: Num a => a
nanoFactor = 1000000000
