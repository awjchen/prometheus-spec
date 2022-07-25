{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Export
  ( tests
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict as M
import Data.Foldable (for_)
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.Export (sampleToPrometheus)
import qualified System.Metrics.Prometheus.Gauge as Gauge
import qualified System.Metrics.Prometheus.Histogram as Histogram
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

-- | Test whether the output of 'sampleToPrometheus is consistent with its
-- Haddocks.
tests :: Spec
tests =
  describe "The output of `sampleToPrometheus`" $
    it "is consistent with its haddocks" $ do

  store <- newStore @ExampleMetrics

  gauge <- createGauge ExampleGauge () store
  Gauge.set gauge 100

  counter1 <-
    createCounter
      ExampleCounter
      (M.fromList [("label.name.1", "label value 1"), ("label.name.2", "label value 1")])
      store
  Counter.add counter1 10

  counter2 <-
    createCounter
      ExampleCounter
      (M.fromList [("label.name.1", "label value 2"), ("label.name.2", "label value 2")])
      store
  Counter.add counter2 11

  let upperBounds = [1, 2, 3]
  histogram <-
    createHistogram
      upperBounds
      ExampleHistogram
      (M.singleton "label_name" "label_value")
      store
  for_ (4:upperBounds) (Histogram.observe histogram)

  prometheusSample <-
    BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

  shouldBe prometheusSample
    "# HELP _100gauge Example gauge\n# TYPE _100gauge gauge\n_100gauge 100.0\n\n# HELP my_counter Example counter\n# TYPE my_counter counter\nmy_counter{label_name_2=\"label value 1\",label_name_1=\"label value 1\"} 10.0\nmy_counter{label_name_2=\"label value 2\",label_name_1=\"label value 2\"} 11.0\n\n# HELP my_histogram Example histogram\n# TYPE my_histogram histogram\nmy_histogram_bucket{le=\"1.0\",label_name=\"label_value\"} 1\nmy_histogram_bucket{le=\"2.0\",label_name=\"label_value\"} 2\nmy_histogram_bucket{le=\"3.0\",label_name=\"label_value\"} 3\nmy_histogram_bucket{le=\"+Inf\",label_name=\"label_value\"} 4\nmy_histogram_sum{label_name=\"label_value\"} 10.0\nmy_histogram_count{label_name=\"label_value\"} 4\n"

data ExampleMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
  ExampleGauge ::
    ExampleMetrics
      "100gauge"
      "Example gauge"
      'GaugeType
      ()
  ExampleCounter ::
    ExampleMetrics
      "my.counter"
      "Example counter"
      'CounterType
      (M.HashMap T.Text T.Text)
  ExampleHistogram ::
    ExampleMetrics
      "my.histogram"
      "Example histogram"
      'HistogramType
      (M.HashMap T.Text T.Text)
