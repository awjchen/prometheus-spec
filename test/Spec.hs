{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Test.Hspec

import qualified Distribution
import qualified State

main :: IO ()
main = hspec $ do
  State.tests
  Distribution.tests
