{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.Hspec

import Control.Monad
import Data.Time.Format as Time
import System.Clock (TimeSpec(..))

import CovenantEyes.Api.Types
import CovenantEyes.Api.Internal.Time

main :: IO ()
main = hspec testTime

testTime = do
  describe "Internal.Time.nanosecAsTimeSpec" $ do
    it "converts nanoseconds to time spec" $ do
      withModifiers $ \modifier-> do
        let f x = fromIntegral (modifier x)
            nanosec `gives` (sec', nsec') = nanosecAsTimeSpec (Nanosec (f nanosec)) `shouldBe` TimeSpec (f sec') (f nsec')
        0                 `gives` (0, 0)
        5                 `gives` (0, 5)
        (nanoFactor*10+5) `gives` (10, 5)

  describe "Internal.Time.timeSpecAsNanosec" $ do
    it "converts time spec to nanosecons" $ do
      withModifiers $ \modifier-> do
        let f x = fromIntegral (modifier x)
            (sec', nsec') `gives`  nanosec = timeSpecAsNanosec (TimeSpec (f sec') (f nsec')) `shouldBe` Nanosec (f nanosec)
        (0,  0) `gives` 0
        (0,  5) `gives` 5
        (10, 5) `gives` (nanoFactor*10+5)

  describe "Internal.Time.addTimeOffset" $ do
    it "adds time spec to UTC date-time" $ do
      addTimeOffset (Nanosec 0) (utc "2015-01-01 00:00:00") `shouldBe` utc "2015-01-01 00:00:00"

      addTimeOffset (Nanosec 1)                  (utc "2015-01-01 00:00:00") `shouldBe` utc "2015-01-01 00:00:00.000000001"
      addTimeOffset (Nanosec $ nanoFactor*10)    (utc "2015-01-01 00:00:00") `shouldBe` utc "2015-01-01 00:00:10"
      addTimeOffset (Nanosec $ nanoFactor*60*10) (utc "2015-01-01 00:00:00") `shouldBe` utc "2015-01-01 00:10:00"

      addTimeOffset (Nanosec $ -1)                    (utc "2015-01-01 00:00:00") `shouldBe` utc "2014-12-31 23:59:59.999999999"
      addTimeOffset (Nanosec $ nanoFactor*10*(-1))    (utc "2015-01-01 00:00:00") `shouldBe` utc "2014-12-31 23:59:50"
      addTimeOffset (Nanosec $ nanoFactor*60*10*(-1)) (utc "2015-01-01 00:00:00") `shouldBe` utc "2014-12-31 23:50:00"

  describe "Entities.calculateServerTime" $ do
    it "adds time to a snapshot" $ do
      let snapshot = CeServerTimeSnapshot (TimeSpec 0 0) (utc "2015-01-01 00:00:00")
      let gives timespec utcStr = calculateServerTime snapshot timespec `shouldBe` utc utcStr
      TimeSpec 0          0  `gives` "2015-01-01 00:00:00"
      TimeSpec 0          1  `gives` "2015-01-01 00:00:00.000000001"
      TimeSpec (60*60)    0  `gives` "2015-01-01 01:00:00"
      TimeSpec 0        (-1) `gives` "2014-12-31 23:59:59.999999999"
      TimeSpec ((-60)*60) 0  `gives` "2014-12-31 23:00:00"

  where
    withModifiers = forM_ [id, negate]
    utc = Time.parseTimeOrError False Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
