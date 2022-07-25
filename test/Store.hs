{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Store
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import qualified Data.HashMap.Strict as HM
import Test.Hspec
import Test.HUnit (assertEqual)

import qualified System.Metrics.Prometheus.Internal.Sample as Sample
import System.Metrics.Prometheus.Internal.Store

tests :: Spec
tests =
  describe "The internal Store interface" $ do
    it "passes a smoke test" test_smokeTest

-- | A test that simply runs functions from the interface to make sure they
-- don't throw errors or never return, that is, that they don't evaluate to
-- bottom.
test_smokeTest :: IO ()
test_smokeTest = do
  result <- race (threadDelay 1000000) smokeTest
  assertEqual "Smoke test took too long" result (Right ())

smokeTest :: IO ()
smokeTest = do
  store <- newStore

  let counterIdentifier = Identifier "ccounter" mempty
      gaugeIdentifier = Identifier "cgauge" mempty
  !_ <- createCounter counterIdentifier "" store
  !_ <- createGauge gaugeIdentifier "" store

  deregistrationHandle <- register store $ mconcat
    [ registerCounter (Identifier "rcounter" mempty) "" (pure 0)
    , registerGauge (Identifier "rgauge" mempty) "" (pure 0)
    , flip registerGroup (pure ()) $ Sample.fromList
        [ ("group", HM.singleton "gcounter" mempty, "", const (Counter 0))
        , ("group", HM.singleton "ggauge" mempty, "", const (Gauge 0))
        ]
    ]

  !_ <- sampleAll store

  deregistrationHandle
