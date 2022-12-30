{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines the internal state of the metrics store and all
-- operations on it.
--
-- = Warning
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
module System.Metrics.Prometheus.Internal.State
    (
      -- * The metric store state
      State
    , MetricSampler (..)
    , GroupSampler (..)
    , Identifier (..)
    , initialState

      -- * State verification
    , verifyState

      -- * Core operations
      -- $core-operations
    , register
    , Mutability (..)
    , registerGroup
    , deregister

      -- * Derived operations
      -- $derived-operations
    , Handle
    , deregisterByHandle

      -- * Query
    , lookupMutability

      -- * Sampling metrics
    , Sample
    , Name
    , Labels
    , Help
    , sampleAll
    , Value(..)

      -- * Testing
    , SampledState (..)
    , sampleState
    , functionallyEqual
    ) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', mapAccumL)
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding (read)
import System.Metrics.Prometheus.Histogram (HistogramSample)
import qualified System.Metrics.Prometheus.Internal.Map2 as M2
import qualified System.Metrics.Prometheus.Internal.Sample as Sample

------------------------------------------------------------------------
-- * The metric store state

-- Internal type aliases
type Name = T.Text
type Labels = HM.HashMap T.Text T.Text
type Help = T.Text
type GroupId = Integer
type Version = Integer

-- | The internal state of the metrics `System.Metrics.Prometheus.Store`.
data State = State
     { stateMetrics ::
        !(M.Map Name (Help, M.Map Labels Metric))
        -- ^ A registry of all metrics in the store.
        --
        -- Metrics are uniquely identified by the combination of their
        -- name and labels. Within the store, metric identifiers are
        -- associated with a metric unless the metric is a part of a
        -- sample group, in which case the identifier is associated with
        -- the group while the group is associated with the metric.
        --
        -- Identifiers are also associated with a `Version` number in
        -- order to differentiate between the different metrics an
        -- identifier may be associated with over time as metrics are
        -- deregistered and reregistered.
     , stateGroups  :: !(M.Map GroupId GroupSampler)
        -- ^ Actions to sample groups of metrics, indexed by `GroupId`.
        --
        -- Invariants: A `GroupSampler` for a group @g@ should sample
        -- for a metric exactly when the metric's identifier is
        -- associated with the group @g@ in the `stateMetrics` registry;
        -- sample groups must sample for at least one metric.
     , stateNextGroupId  :: !GroupId
        -- ^ The `GroupId` to be used for the next registered group.
        --
        -- Invariants: Increases monotonically; must be greater than the
        -- `GroupID`s of all existing sample groups.
     , stateNextMetricVersion :: !Version
        -- ^ The version to be used for the next registered metric.
        --
        -- Invariants: Increases monotonically; must be greater than the
        -- metric versions of all existing metrics.
     }

data Metric = Metric
  { metricGroupOrSampler :: Either MetricSampler GroupId
  , metricVersion :: !Version
  , metricMutabilty :: !Mutability
  }

-- TODO: Rename this to Metric and Metric to SampledMetric.
-- | An action to read the current value of a metric. Needs to be
-- thread-safe.
data MetricSampler
  = CounterS !(IO Double)
    -- ^ Action to sample a counter
  | GaugeS !(IO Double)
    -- ^ Action to sample a gauge
  | HistogramS !(IO HistogramSample)
    -- ^ Action to sample a histogram

-- | Whether a registered metric should be permanent, or allowed to be
-- removed or replaced.

-- Internal note: The mutability of metrics is not enforced in this
-- module. In order to reduce the number of "core" operations, we only
-- implement operations for removable metrics, where a newer metric
-- registered at the same identifier as an existing metric will replace
-- the existing metric.
data Mutability = Permanent | Removable
    deriving (Eq)

-- | An action to sample a group of metrics together.
--
-- Can be useful for efficiency or obtaining a consistent view of
-- multiple metrics. Needs to be thread safe.
--
data GroupSampler = forall a. GroupSampler
     { groupSampleAction :: !(IO a)
        -- ^ Action to sample the metric group
     , groupSamplerMetrics ::
          !(M.Map Name (M.Map Labels (a -> Value)))
        -- ^ Metric identifiers and getter functions.
     }

-- | Metrics are uniquely identified by the combination of their name
-- and labels.
data Identifier = Identifier
    { idName :: T.Text
      -- ^ The name of the metric
    , idLabels :: HM.HashMap T.Text T.Text
      -- ^ The key-value pairs associated with the metric
    }
    deriving (Eq, Generic, Ord, Show)

-- | The initial state of a new store.
initialState :: State
initialState = State M.empty M.empty 0 0

------------------------------------------------------------------------
-- * State verification

-- | Verify the internal consistency of the state.
verifyState :: State -> Bool
verifyState state =
     checkSampleGroups state
  && checkNextGroupId state
  && checkNextMetricVersion state

-- | Check the following invariants:
--
-- A `GroupSampler` for a group @g@ should sample for a metric exactly
-- when the metric's identifier is associated with the group @g@ in the
-- `stateMetrics` registry; sample groups must sample for at least one
-- metric.
checkSampleGroups :: State -> Bool
checkSampleGroups State{..} =
  -- Note: The check for non-empty sample groups is implicit.
  groupsFromGroups == groupsFromMetrics
  where
    groupsFromGroups = groupSamplerIdentifiers <$> stateGroups

    groupsFromMetrics =
      foldl' insert_ M.empty $ do
        (name, (_help, labelsMap)) <- M.toList stateMetrics
        (labels, Metric (Right groupId) _ _) <- M.toList labelsMap
        pure (Identifier name labels, groupId)
      where
        insert_ m (identifier, groupId) =
            M.alter (putIdentifier identifier) groupId m
        putIdentifier identifier =
            Just . maybe (S.singleton identifier) (S.insert identifier)

groupSamplerIdentifiers :: GroupSampler -> S.Set Identifier
groupSamplerIdentifiers GroupSampler{..} =
  S.fromList $ do
    (name, labelsMap) <- M.toList groupSamplerMetrics
    labels <- M.keys labelsMap
    pure $ Identifier name labels

-- | Check the following invariant:
--
-- `stateNextGroupId` must be greater than the `GroupID`s of all existing
-- sample groups.
checkNextGroupId :: State -> Bool
checkNextGroupId State{..} =
    maybe True (< stateNextGroupId) mLargestGroupId
  where
    mLargestGroupId = fst <$> M.lookupMax stateGroups

-- | Check the following invariant:
--
-- `stateNextMetricVersion` must be greater than the metric versions of
-- all existing metrics.
checkNextMetricVersion :: State -> Bool
checkNextMetricVersion State{..} =
    let versions =
          map metricVersion $ M.elems =<< map snd (M.elems stateMetrics)
    in  all (< stateNextMetricVersion) versions

------------------------------------------------------------------------
-- * Core operations

-- $core-operations
-- These "core" operations represent the complete set of ways in which
-- the `State` may be modified. We must make sure that these ops
-- maintain the `State` invariants.

-- | Deregister the metric at the given identifier. When no metric is
-- registered at the identifier, the original state is returned.
deregister
    :: Identifier -- ^ Metric identifier
    -> State
    -> State
deregister (Identifier name labels) state =
    case Sample.lookup name labels (stateMetrics state) of
        Nothing -> state
        Just (Metric (Left _) _ _) -> state
            { stateMetrics =
                Sample.delete name labels (stateMetrics state)
            }
        Just (Metric (Right groupID) _ _) -> state
            { stateMetrics =
                Sample.delete name labels (stateMetrics state)
            , stateGroups =
                let delete_ =
                        overGroupSamplerMetrics
                            (M2.nonEmptyMap . M2.delete name labels)
                in  M.update delete_ groupID (stateGroups state)
            }

overGroupSamplerMetrics ::
  (Functor f) =>
  (forall a.
    M.Map Name (M.Map Labels a) ->
    f (M.Map Name (M.Map Labels a))
    ) ->
  GroupSampler ->
  f GroupSampler
overGroupSamplerMetrics f GroupSampler{..} =
  flip fmap (f groupSamplerMetrics) $ \groupSamplerMetrics' ->
      GroupSampler
          { groupSampleAction = groupSampleAction
          , groupSamplerMetrics = groupSamplerMetrics'
          }

-- | Register a metric at the given identifier. If the identifier is
-- already in use by an existing metric, the existing metric is first
-- removed. Returns a handle for deregistering the registered metric.
register
    :: Identifier -- ^ Metric identifier
    -> Help -- ^ Help text
    -> MetricSampler -- ^ Action to sample the metric
    -> Mutability -- ^ Whether the metric should be registered as permanent or removable
    -> State -- ^ State
    -> (State, Handle) -- ^ (New state, deregistration handle)
register identifier help sample mutability =
  insertMetricSampler identifier help sample mutability
      . deregister identifier

insertMetricSampler
  :: Identifier
  -> Help
  -> MetricSampler
  -> Mutability
  -> State
  -> (State, Handle)
insertMetricSampler identifier help sampler mutability state0 =
  let stateNextMetricVersion0 = stateNextMetricVersion state0
      metric = Metric (Left sampler) stateNextMetricVersion0 mutability
      state1 = state0
        { stateMetrics =
            Sample.insert
              (idName identifier)
              (idLabels identifier)
              help
              metric
              (stateMetrics state0)
        , stateNextMetricVersion = stateNextMetricVersion0 + 1
        }
      handle = Handle identifier stateNextMetricVersion0
  in  (state1, handle)

-- | Register a group of metrics sharing a common sampling action. If
-- any of the given identifiers are in use by an existing metric, the
-- existing metrics are first removed. Returns handles for deregistering
-- the registered metrics. See `System.Metrics.Prometheus.registerGroup`.
registerGroup
    :: M.Map Name (Help, M.Map Labels (a -> Value))
        -- ^ Metric identifiers and getter functions
    -> IO a -- ^ Action to sample the metric group
    -> Mutability -- ^ Whether the metrics should be registered as permanent or removable
    -> State
    -> (State, [Handle])
registerGroup getters cb mutability =
  insertGroup getters cb mutability . delete_
  where
    delete_ state =
      foldl' (flip deregister) state $ do
        (name, (_help, labelsMap)) <- M.toList getters
        labels <- M.keys labelsMap
        pure $ Identifier name labels

insertGroup
    :: M.Map Name (Help, M.Map Labels (a -> Value))
        -- ^ Metric identifiers and getter functions
    -> IO a -- ^ Action to sample the metric group
    -> Mutability -- ^ Whether the metrics should be registered as permanent or removable
    -> State
    -> (State, [Handle])
insertGroup getters cb mutability state0
  | M.null getters = (state0, [])
  | otherwise =
      let getters' = M.map snd getters
          (state1, groupId) =
            insertGroupSampler (GroupSampler cb getters') state0
          insertGroupReference' =
              insertGroupReference groupId mutability
      in  mapAccumL insertGroupReference' state1 $ do
            (name, (help, labelsMap)) <- M.toList getters
            labels <- M.keys labelsMap
            pure (Identifier name labels, help)

insertGroupSampler :: GroupSampler -> State -> (State, GroupId)
insertGroupSampler groupSampler state0 =
  let stateNextGroupId0 = stateNextGroupId state0
      state1 = state0
        { stateGroups =
            M.insert stateNextGroupId0 groupSampler (stateGroups state0)
        , stateNextGroupId = stateNextGroupId0 + 1
        }
  in  (state1, stateNextGroupId0)

insertGroupReference
  :: GroupId
  -> Mutability
  -> State
  -> (Identifier, Help)
  -> (State, Handle)
insertGroupReference groupId mutability state0 (identifier, help) =
  let stateNextMetricVersion0 = stateNextMetricVersion state0
      metric = Metric (Right groupId) stateNextMetricVersion0 mutability
      state1 = state0
        { stateMetrics =
            Sample.insert
              (idName identifier)
              (idLabels identifier)
              help
              metric
              (stateMetrics state0)
        , stateNextMetricVersion = stateNextMetricVersion0 + 1
        }
      handle = Handle identifier stateNextMetricVersion0
  in  (state1, handle)

------------------------------------------------------------------------
-- * Derived operations

-- $derived-operations
-- These "derived" operations must only make modifications to the
-- `State` through the "core" operations. This is so that we can deduce
-- that the derived ops preserve the `State` invariants as long as the
-- core ops do.

-- | A reference to a particular version of the metric at an identifier.
--
-- A value of this type should never be exposed in order to prevent it
-- from being applied to the wrong `State`.
data Handle = Handle Identifier Version

-- | Deregister the particular metric referenced by the handle. That is,
-- deregister the metric at the given identifier, but only if its
-- version matches that held by the `Handle`.
deregisterByHandle :: Handle -> State -> State
deregisterByHandle (Handle identifier version) state =
    let Identifier name labels = identifier
     in case Sample.lookup name labels (stateMetrics state) of
            Nothing -> state
            Just (Metric _ version' _) ->
                if version == version' then
                    deregister identifier state
                else
                    state

------------------------------------------------------------------------
-- * Query

-- | Get the mutability of the metric registered at an identifier, if it
-- exists.
lookupMutability :: Identifier -> State -> Maybe Mutability
lookupMutability (Identifier name labels) state =
  metricMutabilty <$> Sample.lookup name labels (stateMetrics state)

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | A sample of some metrics.
type Sample = M.Map Name (Help, M.Map Labels Value)

-- Internal type alias
type SampleWithoutHelp = M.Map Name (M.Map Labels Value)

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: State -> IO Sample
sampleAll state = do
    let metrics =
            M.map (second (M.map metricGroupOrSampler)) $
                stateMetrics state
        groups = stateGroups state
    groupSamples <- sampleGroups $ M.elems groups
    individualSamples <- readAllRefs metrics
    let allSamples = joinGroupSamples individualSamples groupSamples
    return $! allSamples

-- | Merge together samples of individual metrics (with help text) and
-- samples of grouped metrics (without help text).
joinGroupSamples :: Sample -> SampleWithoutHelp -> Sample
joinGroupSamples =
  Merge.merge
    Merge.preserveMissing
    Merge.dropMissing -- By the invariant checked by `checkSampleGroups`, nothing should be dropped here.
    (Merge.zipWithMatched
      (\_key (help, labelsMap1) labelsMap2 ->
          (help, M.union labelsMap1 labelsMap2)
      ))

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO SampleWithoutHelp
sampleGroups cbSamplers =
  M.unionsWith M.union `fmap` mapM runOne cbSamplers
  where
    runOne :: GroupSampler -> IO SampleWithoutHelp
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! M.map (M.map ($ a)) groupSamplerMetrics

-- | The value of a sampled metric.
data Value = Counter {-# UNPACK #-} !Double
           | Gauge {-# UNPACK #-} !Double
           | Histogram !HistogramSample
           deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Value
sampleOne (CounterS m)      = Counter <$> m
sampleOne (GaugeS m)        = Gauge <$> m
sampleOne (HistogramS m)    = Histogram <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.

-- Implementation note: This function assumes that, for each metric name
-- in the provided map, the associated map is non-empty.
readAllRefs
  :: M.Map Name (Help, M.Map Labels (Either MetricSampler GroupId))
  -> IO Sample
readAllRefs =
  M.traverseWithKey $ \_k ->
    traverse $
      M.traverseMaybeWithKey $ \_k v ->
        case v of
          Left ref -> Just <$> sampleOne ref
          Right _ -> pure Nothing

------------------------------------------------------------------------
-- * Testing

-- | A version of `State` where the samplers have been replaced by their
-- results. Intended to be used in tests where the samplers are all of
-- the form @pure x@ for some @x@.
data SampledState = SampledState
     { sampledStateMetrics ::
        !(M.Map Name
            (Help, M.Map Labels (Either Value GroupId, Version)))
     , sampledStateGroups :: !(M.Map GroupId SampleWithoutHelp)
     , sampledStateNextGroupId :: !GroupId
     , sampledStateNextMetricVersion :: !Version
     }
  deriving (Show, Eq)

-- | Run all the sampling actions in the state and replace them with
-- their results.
sampleState :: State -> IO SampledState
sampleState State{..} = do
  sampledMetrics <-
    traverse (traverse (traverse sampleMetric)) stateMetrics
  sampledGroups <- traverse sampleGroupSampler stateGroups
  pure $ SampledState
     { sampledStateMetrics = sampledMetrics
     , sampledStateGroups = sampledGroups
     , sampledStateNextGroupId = stateNextGroupId
     , sampledStateNextMetricVersion = stateNextMetricVersion
     }
  where
    sampleMetric
      :: Metric
      -> IO (Either Value GroupId, Version)
    sampleMetric (Metric (Right groupId) version _) =
      pure (Right groupId, version)
    sampleMetric (Metric (Left sample) version _) = do
      value <- sampleOne sample
      pure (Left value, version)

    sampleGroupSampler :: GroupSampler -> IO SampleWithoutHelp
    sampleGroupSampler GroupSampler{..} =
      (\r -> M.map (M.map ($ r)) groupSamplerMetrics)
        <$> groupSampleAction

-- | Test for equality ignoring `MetricId`s and `GroupId`s.
--
-- This test assumes that each `GroupSampler` in `stateGroups` is
-- unique, which follows from the `State` invariants.
functionallyEqual :: SampledState -> SampledState -> Bool
functionallyEqual state1 state2 = withoutIds state1 == withoutIds state2
  where
    withoutIds
      :: SampledState
      -> M.Map Name
          (Help, M.Map Labels (Either Value (Maybe SampleWithoutHelp)))
    withoutIds state =
      flip (M.map . second . M.map) (sampledStateMetrics state) $
        \(e, _version) -> case e of
          Left value -> Left value
          Right groupId ->
            Right $ M.lookup groupId (sampledStateGroups state)
