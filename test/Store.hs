{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Store
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Test.Hspec
import Test.HUnit (assertEqual)

import qualified System.Metrics.Prometheus.Internal.Sample as Sample
import System.Metrics.Prometheus.Internal.Store

tests :: Spec
tests =
  describe "The internal Store interface" $ do
    it "passes a smoke test" test_smokeTest
    it "throws exceptions on invalid input" test_validation

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
      histogramIdentifier = Identifier "chistogram" mempty
  !_ <- createCounter counterIdentifier "" store
  !_ <- createGauge gaugeIdentifier "" store
  !_ <- createHistogram [] histogramIdentifier "" store

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

-- | Basic test of the store's input validation.
test_validation :: IO ()
test_validation = do
  store <- newStore

  registerSomething store "validMetricName" "validHelpText" "validLabelName"
  -- should not throw an exception

  registerSomething store "0invalidMetricName" "validHelpText" "validLabelName"
    `shouldThrow` \case InvalidMetricName _ -> True; _ -> False

  registerSomething store "validMetricName" "invalidHelpText\\t" "validLabelName"
    `shouldThrow` \case InvalidHelpText _ -> True; _ -> False

  registerSomething store "validMetricName" "validHelpText" "\"invalidLabelName"
    `shouldThrow` \case InvalidLabelName _ -> True; _ -> False

registerSomething :: Store -> Text -> Text -> Text -> IO ()
registerSomething store metricName helpText labelName =
  void $
    register store $
      let identifier =
            Identifier metricName (HM.singleton labelName "labelValue")
      in registerCounter identifier helpText (pure 0)
