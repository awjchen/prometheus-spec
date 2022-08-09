module System.Metrics.Prometheus.Validation
  ( ValidationError (..)
  , isValidName
  ) where

import Control.Exception
import Data.Char (isDigit)
import qualified Data.Text as T

data ValidationError
  = InvalidMetricName T.Text
  | InvalidLabelName T.Text

instance Exception ValidationError

instance Show ValidationError where
  show (InvalidMetricName invalidName) =
    "Invalid Prometheus metric name: " ++ T.unpack invalidName
  show (InvalidLabelName invalidName) =
    "Invalid Prometheus label name: " ++ T.unpack invalidName

-- | Test whether a string is a valid Prometheus name by checking that it
-- matches the regex @[a-zA-Z_][a-zA-Z0-9_]*@.
-- See
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
-- for more details.
--
isValidName :: T.Text -> Bool
isValidName name =
    case T.uncons name of
        Nothing -> False
        Just (headChar, _tail) ->
            isInitialNameChar headChar && T.all isNameChar name

isNameChar :: Char -> Bool
isNameChar c = isInitialNameChar c || isDigit c

isInitialNameChar :: Char -> Bool
isInitialNameChar c =
  'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
