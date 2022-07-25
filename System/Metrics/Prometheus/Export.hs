{-
This module is based on code from the prometheus-2.2.3 package:
https://hackage.haskell.org/package/prometheus-2.2.3

The license for prometheus-2.2.3 follows:

BSD 3-Clause License

Copyright (c) 2016-present, Bitnomial, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines an exporter for rendering metric 'Sample's into the
-- Prometheus format. To help ensure that the output of the exporter conforms
-- to the Prometheus format, the exporter peforms some sanitization of its
-- input.
--
module System.Metrics.Prometheus.Export
  ( sampleToPrometheus
  , escapeLabelValue
  ) where

import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Builder as B
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Metrics.Prometheus
  ( Sample,
    Value (Counter, Gauge, Histogram),
  )
import System.Metrics.Prometheus.Histogram
  ( HistogramSample (histBuckets, histCount, histSum)
  )

------------------------------------------------------------------------------

-- | Encode a metrics 'Sample' into the Prometheus 2 exposition format,
-- adjusting the sample as follows:
--
-- * The names of metrics and labels are adjusted to be valid Prometheus
-- names (see
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>):
--
--     * characters not matched by the regex @[a-zA-Z0-9_]@ are replaced
--     with an underscore (@_@), and
--
--     * if a name is empty or if its first character is not matched by
--     the regex @[a-zA-Z_]@, it is prefixed with an underscore (@_@).
--
-- Note that, for the output of this function to be a valid Prometheus
-- metric exposition, the 'Sample' you provide must meet the following
-- conditions:
--
--
-- * Within the values of labels, the backslash (@\\@), double-quote
-- (@\"@), and line feed (@\\n@) characters must be escaped as @\\\\@,
-- @\\\"@, and @\\n@, respectively. See 'escapeLabelValue'.
--
-- * If two metrics have the same name, they must also have the same
-- metric type.
--
--     * For each name, only metrics of one type (chosen arbitrarily)
--     will be retained while those of other types will be discarded.
--
--
-- For example, a metrics sample consisting of
--
-- (1) a metric named @100gauge@, with no labels, of type @Gauge@, with
-- value @100@;
-- (2) a metric named @my.counter@, with labels @{label.name.1="label value 1", label.name.2="label value 1"}@, of type
-- @Counter@, with value @10@;
-- (3) a metric named @my.counter@, with labels @{label.name.1="label value 2", label.name.2="label value 2"}@, of type
-- @Counter@, with value @11@;
-- (4) a metric named @my.histogram@, with labels @{label_name="label_value"}@, of type
-- @Histogram@, with bucket upper bounds of @[1, 2, 3]@, and
-- observations @[1, 2, 3, 4]@;
--
-- is encoded as follows:
--
-- > # TYPE _100gauge gauge
-- > _100gauge 100.0
-- >
-- > # TYPE my_counter counter
-- > my_counter{label_name_2=\"label value 1\",label_name_1=\"label value 1\"} 10.0
-- > my_counter{label_name_2=\"label value 2\",label_name_1=\"label value 2\"} 11.0
-- >
-- > # TYPE my_histogram histogram
-- > my_histogram_bucket{le=\"1.0\",label_name=\"label_value\"} 1
-- > my_histogram_bucket{le=\"2.0\",label_name=\"label_value\"} 2
-- > my_histogram_bucket{le=\"3.0\",label_name=\"label_value\"} 3
-- > my_histogram_bucket{le=\"+Inf\",label_name=\"label_value\"} 4
-- > my_histogram_sum{label_name=\"label_value\"} 10.0
-- > my_histogram_count{label_name=\"label_value\"} 4
--
sampleToPrometheus :: Sample -> B.Builder
sampleToPrometheus =
  mconcat
    . intersperse newline
    . map exportGroupedMetric
    . mapMaybe
        ( makeGroupedMetric
        . bimap
            sanitizeName
            (map (first (HM.mapKeys sanitizeName)) . M.toAscList)
        )
    . M.toAscList

type Labels = HM.HashMap T.Text T.Text

------------------------------------------------------------------------------

data GroupedMetric
  = GroupedCounter
      T.Text -- Metric name
      [(Labels, Double)]
  | GroupedGauge
      T.Text -- Metric name
      [(Labels, Double)]
  | GroupedHistogram
      T.Text -- Metric name
      [(Labels, HistogramSample)]

-- Within a sample, metrics with the name should have the same metric
-- type, but this is not guaranteed. To handle the case where two
-- metrics have the same name but different metric types, we arbitrarily
-- choose, for each metric name, one of its metric types, and discard
-- all the metrics of that name that do not have that type.
--
makeGroupedMetric ::
  (T.Text, [(Labels, Value)]) -> Maybe GroupedMetric
makeGroupedMetric (metricName, pairs@((_, headVal):_)) =
  Just $
    case headVal of
      Counter _ ->
        GroupedCounter metricName $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getCounterValue val)
      Gauge _ ->
        GroupedGauge metricName $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getGaugeValue val)
      Histogram _ ->
        GroupedHistogram metricName $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getHistogramValue val)
makeGroupedMetric _ = Nothing

getCounterValue :: Value -> Maybe Double
getCounterValue = \case
  Counter x -> Just x
  _ -> Nothing

getGaugeValue :: Value -> Maybe Double
getGaugeValue = \case
  Gauge x -> Just x
  _ -> Nothing

getHistogramValue :: Value -> Maybe HistogramSample
getHistogramValue = \case
  Histogram hs -> Just hs
  _ -> Nothing

------------------------------------------------------------------------------

exportGroupedMetric :: GroupedMetric -> B.Builder
exportGroupedMetric = \case
  GroupedCounter metricName labelsAndValues ->
    exportCounter metricName labelsAndValues
  GroupedGauge metricName labelsAndValues ->
    exportGauge metricName labelsAndValues
  GroupedHistogram metricName labelsAndValues ->
    exportHistogram metricName labelsAndValues

-- Prometheus counter samples
exportCounter :: T.Text -> [(Labels, Double)] -> B.Builder
exportCounter metricName labelsAndValues =
  mappend (metricTypeLine "counter" metricName) $
    foldMap
      (\(labels, value) ->
        metricSampleLine metricName labels (double value))
      labelsAndValues

-- Prometheus gauge samples
exportGauge :: T.Text -> [(Labels, Double)] -> B.Builder
exportGauge metricName labelsAndValues =
  mappend (metricTypeLine "gauge" metricName) $
    foldMap
      (\(labels, value) ->
        metricSampleLine metricName labels (double value))
      labelsAndValues

-- Prometheus histogram samples
exportHistogram :: T.Text -> [(Labels, HistogramSample)] -> B.Builder
exportHistogram metricName labelsAndValues =
  mappend (metricTypeLine "histogram" metricName) $
    flip foldMap labelsAndValues $ \(labels, histSample) ->
      mconcat
        [ let cumulativeBuckets =
                snd $ M.mapAccum cumulativeSum 0 (histBuckets histSample)
                where
                  cumulativeSum !sum_ x = let z = sum_ + x in (z, z)
           in flip foldMap (M.toList cumulativeBuckets) $
                \(upperBound, count) ->
                  metricSampleLine
                    metricName_bucket
                    (HM.insert "le" (T.pack (show upperBound)) labels)
                    (int count)
        , metricSampleLine
            metricName_bucket
            (HM.insert "le" "+Inf" labels)
            (int (histCount histSample))
        , metricSampleLine
            (metricName <> "_sum")
            labels
            (double (histSum histSample))
        , metricSampleLine
            (metricName <> "_count")
            labels
            (int (histCount histSample))
        ]
  where
    metricName_bucket = metricName <> "_bucket"

------------------------------------------------------------------------------

-- Prometheus metric type line
metricTypeLine :: B.Builder -> T.Text -> B.Builder
metricTypeLine metricType metricName =
  "# TYPE "
    <> text metricName
    <> B.charUtf8 ' '
    <> metricType
    <> newline

-- Prometheus metric sample line
metricSampleLine ::
  T.Text -> HM.HashMap T.Text T.Text -> B.Builder -> B.Builder
metricSampleLine metricName labels value =
  text metricName
    <> labelSet labels
    <> B.charUtf8 ' '
    <> value
    <> newline

-- Prometheus label set
labelSet :: HM.HashMap T.Text T.Text -> B.Builder
labelSet labels
  | HM.null labels = mempty
  | otherwise =
      let labelList =
            mconcat $
              intersperse (B.charUtf8 ',') $
                map labelPair $
                  HM.toList labels
       in B.charUtf8 '{'
            <> labelList
            <> B.charUtf8 '}'

-- Prometheus name-value label pair
labelPair :: (T.Text, T.Text) -> B.Builder
labelPair (labelName, labelValue) =
  text labelName
    <> B.charUtf8 '='
    <> B.charUtf8 '"'
    <> text labelValue
    <> B.charUtf8 '"'

------------------------------------------------------------------------------
-- Input sanitization

-- | Adjust a string so that it is a valid Prometheus name:
--
-- * Characters not matched by the regex @[a-zA-Z0-9_]@ are replaced
-- with an underscore (@_@); and
--
-- * If a name is empty or if its first character is not matched by the
-- regex @[a-zA-Z_]@, it is prefixed with an underscore (@_@).
--
-- This function is idempotent.
--
-- See
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
-- for more details.
--
sanitizeName :: T.Text -> T.Text
sanitizeName rawName =
  case T.uncons rawName of
    Nothing -> "_"
    Just (headChar, _tail) -> prefix <> sanitizedName
      where
        prefix = if isInitialNameChar headChar then "" else "_"
        sanitizedName = T.map (\c -> if isNameChar c then c else '_') rawName

isNameChar :: Char -> Bool
isNameChar c = isInitialNameChar c || isDigit c

isInitialNameChar :: Char -> Bool
isInitialNameChar c =
  'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'

-- | A convenience function for escaping the backslash (@\\@),
-- double-quote (@\"@), and line feed (@\\n@) characters of a string as
-- @\\\\@, @\\\"@, and @\\n@, respectively, so that it is a valid
-- Prometheus label value. This function is not idempotent.
--
-- See
-- <https://prometheus.io/docs/instrumenting/exposition_formats/#comments-help-text-and-type-information>.
--
-- > >>> putStrLn $ escapeLabelValue "\n \" \\"
-- > \n \" \\

-- Implementation note: We do not apply this function on behalf of the
-- user because it is not idempotent.
escapeLabelValue :: T.Text -> T.Text
escapeLabelValue = T.concatMap escapeLabelValueChar

escapeLabelValueChar :: Char -> T.Text
escapeLabelValueChar c
  | c == '\n' = "\\n"
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | otherwise = T.singleton c

------------------------------------------------------------------------------
-- Builder helpers

text :: T.Text -> B.Builder
text = B.byteString . T.encodeUtf8

double :: Double -> B.Builder
double = B.stringUtf8 . show

int :: Int64 -> B.Builder
int = B.stringUtf8 . show

newline :: B.Builder
newline = B.charUtf8 '\n'
