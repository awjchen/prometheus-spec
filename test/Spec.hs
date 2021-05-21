{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Control.Monad (void)
import Data.Coerce
import Data.Foldable (asum)
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Generic.Random (genericArbitrary, uniform)
import GHC.Generics
import System.Exit
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Drivers as SC
import qualified Test.SmallCheck.Series as SC

import System.Metrics.Internal

------------------------------------------------------------------------
-- * Testing state operations
--
-- We generate state operations (e.g. register, deregister) on a
-- restricted space of identifiers so that the operations are more
-- likely to interact.

-- ** Restricted inputs

names :: [T.Text]
names = ["a", "b"]

tagSets :: [M.HashMap T.Text T.Text]
tagSets = [M.singleton "k" "v", M.singleton "k" "w"]

identifiers :: [Identifier]
identifiers = Identifier <$> names <*> tagSets
identifierGroups :: [[Identifier]]
identifierGroups =
  [ []
  , [a]
  , [a, b], [a, c], [b, c]
  , [a, b, c]
  ]
  where
    (a:b:c:_) = identifiers

samplingGroups :: [M.HashMap Identifier (() -> Value)]
samplingGroups = map (M.fromList . map (, sample)) identifierGroups
  where sample = const (Counter 0)

-- ** State operation representation

-- | A representation of all state operations, ignoring sampling
-- actions.
data TestStateOp
  = Register Identifier
  | RegisterGroup (M.HashMap Identifier (() -> Value))
  | Deregister Identifier
  | DeregisterByName T.Text

-- | Realize the state operations (using phony sampling actions).
runTestStateOp :: TestStateOp -> State -> State
runTestStateOp op = case op of
  Register identifier -> register identifier (CounterS (pure 0))
  RegisterGroup group -> registerGroup group (pure ())
  Deregister identifier -> deregister identifier
  DeregisterByName name -> deregisterByName name

instance Show TestStateOp where
  show (Register id') = "Register (" ++ show id' ++ ")"
  show (RegisterGroup idGroup) = "RegisterGroup " ++ show (M.keys idGroup)
  show (Deregister id') = "Deregister (" ++ show id' ++ ")"
  show (DeregisterByName name) = "DeregisterByName " ++ show name

instance (Monad m) => SC.Serial m TestStateOp where
  series = asum
    [ Register <$> choose identifiers
    , RegisterGroup <$> choose samplingGroups
    , Deregister <$> choose identifiers
    , DeregisterByName <$> choose names
    ]
    where
      choose :: (Alternative f) => [a] -> f a
      choose = foldr ((<|>) . pure) empty

instance QC.Arbitrary TestStateOp where
  -- | Frequencies are biased towards registration but are otherwise
  -- arbitrary
  arbitrary = QC.frequency
    [ (4, Register <$> QC.elements identifiers)
    , (4, RegisterGroup <$> QC.elements samplingGroups)
    , (2, Deregister <$> QC.elements identifiers)
    , (1, DeregisterByName <$> QC.elements names)
    ]

------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "A sequence of operations on the internal state" $ do
    let verifyOps :: [TestStateOp] -> Bool
        verifyOps ops =
          verifyState $ foldl' (flip runTestStateOp) initialState ops
    it "preserves internal consistency (smallcheck)" $
      -- A depth of 4 yields sequences of operations up to length 3.
      -- The test takes too long if we go any deeper.
      SC.property $ SC.changeDepth (const 4) $ SC.forAll verifyOps
    it "preserves internal consistency (quickcheck)" $
      QC.property verifyOps
