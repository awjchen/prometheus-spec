{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- This module defines the metrics store and all of its operations using
-- the state type defined in "System.Metrics.Internal.State". The
-- interface presented in this module is then restricted in
-- "System.Metrics.Static" to produce the final interface.
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
--
-- = Implementation summary
--
-- * We wrap the internal `State` in an `IORef`, making it suitable as a
--   global store.
--
-- * We wrap operations on the `State` and allow them to be composed,
--   then run such compositions atomically using `atomicModifyIORef'`.
--   This allows for atomic operations on the `Store`.
--
-- * We bind the `Handle`s of "System.Metrics.Internal.State" to
--   specific `IORef`s in `deregisterHandles`, preventing the confusion of
--   handles from different `Store`s.

module System.Metrics.Internal.Store
    (
      -- * The metric store
      -- $metric-store
      Store
    , newStore

      -- * Identifying metrics
    , Identifier (..)

      -- * Registering metrics
      -- $registering
    , Registration
    , register
    , registerCounter
    , registerGauge
    , registerLabel
    , registerDistribution
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createLabel
    , createDistribution

      -- ** Predefined metrics
      -- $predefined
    , registerGcMetrics

      -- * Deregistering metrics
    , Deregistration
    , deregister
    , deregisterMetric
    , deregisterByName

      -- * Sampling metrics
      -- $sampling
    , Sample
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Text as T
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Internal.State
  hiding (deregister, deregisterByName, register, registerGroup, sampleAll)
import qualified System.Metrics.Internal.State as Internal
import System.Metrics.Label (Label)
import qualified System.Metrics.Label as Label

------------------------------------------------------------------------
-- * The metric store

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = Store <$> newIORef initialState

------------------------------------------------------------------------
-- * Registering metrics

-- | An action that registers one or more metrics to a metric store.
newtype Registration =
  Registration (State -> (State, [Handle] -> [Handle]))

instance Semigroup Registration where
  Registration f <> Registration g = Registration $ \state0 ->
    let (state1, h1) = f state0
        (state2, h2) = g state1
    in  (state2, h2 . h1)

-- | Atomically apply a registration action to a metrics store. Returns
-- an action to (atomically) deregisterMetric the newly registered metrics.
register
  :: Store -- ^ Metric store
  -> Registration -- ^ Registration action
  -> IO (IO ()) -- ^ Deregistration action
register (Store stateRef) (Registration f) =
    atomicModifyIORef' stateRef $ \state0 ->
        let (state1, handles') = f state0
            deregisterAction = deregisterHandles (handles' []) stateRef
        in  (state1, deregisterAction)

-- | Deregister the metrics referred to by the given handles.
deregisterHandles
  :: [Internal.Handle]
  -> IORef Internal.State
  -> IO ()
deregisterHandles handles stateRef =
    atomicModifyIORef' stateRef $ \state ->
        (foldl' (flip Internal.deregisterByHandle) state handles, ())

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter :: Identifier -- ^ Counter identifier
                -> IO Int64   -- ^ Action to read the current metric value
                -> Registration -- ^ Registration action
registerCounter identifier sample =
    registerGeneric identifier (CounterS sample)

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge :: Identifier -- ^ Gauge identifier
              -> IO Int64   -- ^ Action to read the current metric value
              -> Registration -- ^ Registration action
registerGauge identifier sample =
    registerGeneric identifier (GaugeS sample)

-- | Register a text metric. The provided action to read the value
-- must be thread-safe. Also see 'createLabel'.
registerLabel :: Identifier -- ^ Label identifier
              -> IO T.Text  -- ^ Action to read the current metric value
              -> Registration -- ^ Registration action
registerLabel identifier sample =
    registerGeneric identifier (LabelS sample)

-- | Register a distribution metric. The provided action to read the
-- value must be thread-safe. Also see 'createDistribution'.
registerDistribution
    :: Identifier             -- ^ Distribution identifier
    -> IO Distribution.Stats  -- ^ Action to read the current metric
    -> Registration -- ^ Registration action
registerDistribution identifier sample =
    registerGeneric identifier (DistributionS sample)

registerGeneric
  :: Identifier -- ^ Metric identifier
  -> MetricSampler -- ^ Sampling action
  -> Registration -- ^ Registration action
registerGeneric identifier sample = Registration $ \state0 ->
    let (state1, handle) = Internal.register identifier sample state0
    in  (state1, (:) handle)

-- | Register an action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in \"bulk\", as the run-time system provides a function to sample all
-- GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the provided
-- action computes @a@ atomically (e.g. if @a@ is a record, the action
-- needs to compute its fields atomically if the sampling is to be
-- atomic.)
--
-- Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.HashMap.Strict as M
-- > import GHC.Stats
-- > import System.Metrics
-- >
-- > main = do
-- >     store <- newStore
-- >     let metrics =
-- >             [ ("num_gcs", Counter . numGcs)
-- >             , ("max_bytes_used", Gauge . maxBytesUsed)
-- >             ]
-- >     registerGroup (M.fromList metrics) getGCStats store
registerGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric names and getter functions.
    -> IO a          -- ^ Action to sample the metric group
    -> Registration -- ^ Registration action
registerGroup getters cb = Registration $ \state0 ->
    let (state1, handles) = Internal.registerGroup getters cb state0
    in  (state1, (++) handles)

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a 'Counter') with registering that reference in the store in one
-- convenient function.
--
-- Deregistration actions are not available through these functions.

-- | Create and register a zero-initialized counter.
createCounter :: Identifier -- ^ Counter identifier
              -> Store      -- ^ Metric store
              -> IO Counter
createCounter identifier store = do
    counter <- Counter.new
    _ <- register store $
          registerCounter identifier (Counter.read counter)
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: Identifier -- ^ Gauge identifier
            -> Store      -- ^ Metric store
            -> IO Gauge
createGauge identifier store = do
    gauge <- Gauge.new
    _ <- register store $
          registerGauge identifier (Gauge.read gauge)
    return gauge

-- | Create and register an empty label.
createLabel :: Identifier -- ^ Label identifier
            -> Store      -- ^ Metric store
            -> IO Label
createLabel identifier store = do
    label <- Label.new
    _ <- register store $
          registerLabel identifier (Label.read label)
    return label

-- | Create and register an event tracker.
createDistribution :: Identifier -- ^ Distribution identifier
                   -> Store      -- ^ Metric store
                   -> IO Distribution
createDistribution identifier store = do
    event <- Distribution.new
    _ <- register store $
          registerDistribution identifier (Distribution.read event)
    return event

------------------------------------------------------------------------
-- ** Predefined metrics

-- $predefined
-- This library provides a number of pre-defined metrics that can
-- easily be added to a metrics store by calling their register
-- function.

#if MIN_VERSION_base(4,10,0)
-- | Convert nanoseconds to milliseconds.
nsToMs :: Int64 -> Int64
nsToMs s = round (realToFrac s / (1000000.0 :: Double))
#else
-- | Convert seconds to milliseconds.
sToMs :: Double -> Int64
sToMs s = round (s * 1000.0)
#endif

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- To enable GC statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.
--
-- Registered counters:
--
-- [@rts.gc.bytes_allocated@] Total number of bytes allocated
--
-- [@rts.gc.num_gcs@] Number of garbage collections performed
--
-- [@rts.gc.num_bytes_usage_samples@] Number of byte usage samples taken
--
-- [@rts.gc.cumulative_bytes_used@] Sum of all byte usage samples, can be
-- used with @numByteUsageSamples@ to calculate averages with
-- arbitrary weighting (if you are sampling this record multiple
-- times).
--
-- [@rts.gc.bytes_copied@] Number of bytes copied during GC
--
-- [@rts.gc.init_cpu_ms@] CPU time used by the init phase, in
-- milliseconds. GHC 8.6+ only.
--
-- [@rts.gc.init_wall_ms@] Wall clock time spent running the init
-- phase, in milliseconds. GHC 8.6+ only.
--
-- [@rts.gc.mutator_cpu_ms@] CPU time spent running mutator threads,
-- in milliseconds. This does not include any profiling overhead or
-- initialization.
--
-- [@rts.gc.mutator_wall_ms@] Wall clock time spent running mutator
-- threads, in milliseconds. This does not include initialization.
--
-- [@rts.gc.gc_cpu_ms@] CPU time spent running GC, in milliseconds.
--
-- [@rts.gc.gc_wall_ms@] Wall clock time spent running GC, in
-- milliseconds.
--
-- [@rts.gc.cpu_ms@] Total CPU time elapsed since program start, in
-- milliseconds.
--
-- [@rts.gc.wall_ms@] Total wall clock time elapsed since start, in
-- milliseconds.
--
-- Registered gauges:
--
-- [@rts.gc.max_bytes_used@] Maximum number of live bytes seen so far
--
-- [@rts.gc.current_bytes_used@] Current number of live bytes
--
-- [@rts.gc.current_bytes_slop@] Current number of bytes lost to slop
--
-- [@rts.gc.max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@rts.gc.peak_megabytes_allocated@] Maximum number of megabytes allocated
--
-- [@rts.gc.par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@rts.gc.par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
--
-- [@rts.gc.par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC. The ratio of
-- @par_tot_bytes_copied@ divided by @par_max_bytes_copied@ approaches
-- 1 for a maximally sequential run and approaches the number of
-- threads (set by the RTS flag @-N@) for a maximally parallel run.
--
registerGcMetrics :: Registration
registerGcMetrics =
    let taglessId :: T.Text -> Identifier
        taglessId name = Identifier name mempty in
    registerGroup
#if MIN_VERSION_base(4,10,0)
    (M.fromList
     [ (taglessId "rts.gc.bytes_allocated"          , Counter . fromIntegral . Stats.allocated_bytes)
     , (taglessId "rts.gc.num_gcs"                  , Counter . fromIntegral . Stats.gcs)
     , (taglessId "rts.gc.num_bytes_usage_samples"  , Counter . fromIntegral . Stats.major_gcs)
     , (taglessId "rts.gc.cumulative_bytes_used"    , Counter . fromIntegral . Stats.cumulative_live_bytes)
     , (taglessId "rts.gc.bytes_copied"             , Counter . fromIntegral . Stats.copied_bytes)
#if MIN_VERSION_base(4,12,0)
     , (taglessId "rts.gc.init_cpu_ms"              , Counter . nsToMs . Stats.init_cpu_ns)
     , (taglessId "rts.gc.init_wall_ms"             , Counter . nsToMs . Stats.init_elapsed_ns)
#endif
     , (taglessId "rts.gc.mutator_cpu_ms"           , Counter . nsToMs . Stats.mutator_cpu_ns)
     , (taglessId "rts.gc.mutator_wall_ms"          , Counter . nsToMs . Stats.mutator_elapsed_ns)
     , (taglessId "rts.gc.gc_cpu_ms"                , Counter . nsToMs . Stats.gc_cpu_ns)
     , (taglessId "rts.gc.gc_wall_ms"               , Counter . nsToMs . Stats.gc_elapsed_ns)
     , (taglessId "rts.gc.cpu_ms"                   , Counter . nsToMs . Stats.cpu_ns)
     , (taglessId "rts.gc.wall_ms"                  , Counter . nsToMs . Stats.elapsed_ns)
     , (taglessId "rts.gc.max_bytes_used"           , Gauge . fromIntegral . Stats.max_live_bytes)
     , (taglessId "rts.gc.current_bytes_used"       , Gauge . fromIntegral . Stats.gcdetails_live_bytes . Stats.gc)
     , (taglessId "rts.gc.current_bytes_slop"       , Gauge . fromIntegral . Stats.gcdetails_slop_bytes . Stats.gc)
     , (taglessId "rts.gc.max_bytes_slop"           , Gauge . fromIntegral . Stats.max_slop_bytes)
     , (taglessId "rts.gc.peak_megabytes_allocated" , Gauge . fromIntegral . (`quot` (1024*1024)) . Stats.max_mem_in_use_bytes)
     , (taglessId "rts.gc.par_tot_bytes_copied"     , Gauge . fromIntegral . Stats.par_copied_bytes)
     , (taglessId "rts.gc.par_avg_bytes_copied"     , Gauge . fromIntegral . Stats.par_copied_bytes)
     , (taglessId "rts.gc.par_max_bytes_copied"     , Gauge . fromIntegral . Stats.cumulative_par_max_copied_bytes)
     ])
    getRTSStats
#else
    (M.fromList
     [ (taglessId "rts.gc.bytes_allocated"          , Counter . Stats.bytesAllocated)
     , (taglessId "rts.gc.num_gcs"                  , Counter . Stats.numGcs)
     , (taglessId "rts.gc.num_bytes_usage_samples"  , Counter . Stats.numByteUsageSamples)
     , (taglessId "rts.gc.cumulative_bytes_used"    , Counter . Stats.cumulativeBytesUsed)
     , (taglessId "rts.gc.bytes_copied"             , Counter . Stats.bytesCopied)
     , (taglessId "rts.gc.mutator_cpu_ms"           , Counter . sToMs . Stats.mutatorCpuSeconds)
     , (taglessId "rts.gc.mutator_wall_ms"          , Counter . sToMs . Stats.mutatorWallSeconds)
     , (taglessId "rts.gc.gc_cpu_ms"                , Counter . sToMs . Stats.gcCpuSeconds)
     , (taglessId "rts.gc.gc_wall_ms"               , Counter . sToMs . Stats.gcWallSeconds)
     , (taglessId "rts.gc.cpu_ms"                   , Counter . sToMs . Stats.cpuSeconds)
     , (taglessId "rts.gc.wall_ms"                  , Counter . sToMs . Stats.wallSeconds)
     , (taglessId "rts.gc.max_bytes_used"           , Gauge . Stats.maxBytesUsed)
     , (taglessId "rts.gc.current_bytes_used"       , Gauge . Stats.currentBytesUsed)
     , (taglessId "rts.gc.current_bytes_slop"       , Gauge . Stats.currentBytesSlop)
     , (taglessId "rts.gc.max_bytes_slop"           , Gauge . Stats.maxBytesSlop)
     , (taglessId "rts.gc.peak_megabytes_allocated" , Gauge . Stats.peakMegabytesAllocated)
     , (taglessId "rts.gc.par_tot_bytes_copied"     , Gauge . gcParTotBytesCopied)
     , (taglessId "rts.gc.par_avg_bytes_copied"     , Gauge . gcParTotBytesCopied)
     , (taglessId "rts.gc.par_max_bytes_copied"     , Gauge . Stats.parMaxBytesCopied)
     ])
    getGcStats
#endif

#if MIN_VERSION_base(4,10,0)
-- | Get RTS statistics.
getRTSStats :: IO Stats.RTSStats
getRTSStats = do
    enabled <- Stats.getRTSStatsEnabled
    if enabled
        then Stats.getRTSStats
        else return emptyRTSStats

-- | Empty RTS statistics, as if the application hasn't started yet.
emptyRTSStats :: Stats.RTSStats
emptyRTSStats = Stats.RTSStats
    { gcs                                  = 0
    , major_gcs                            = 0
    , allocated_bytes                      = 0
    , max_live_bytes                       = 0
    , max_large_objects_bytes              = 0
    , max_compact_bytes                    = 0
    , max_slop_bytes                       = 0
    , max_mem_in_use_bytes                 = 0
    , cumulative_live_bytes                = 0
    , copied_bytes                         = 0
    , par_copied_bytes                     = 0
    , cumulative_par_max_copied_bytes      = 0
# if MIN_VERSION_base(4,11,0)
    , cumulative_par_balanced_copied_bytes = 0
# if MIN_VERSION_base(4,12,0)
    , init_cpu_ns                          = 0
    , init_elapsed_ns                      = 0
# endif
# endif
    , mutator_cpu_ns                       = 0
    , mutator_elapsed_ns                   = 0
    , gc_cpu_ns                            = 0
    , gc_elapsed_ns                        = 0
    , cpu_ns                               = 0
    , elapsed_ns                           = 0
    , gc                                   = emptyGCDetails
    }

emptyGCDetails :: Stats.GCDetails
emptyGCDetails = Stats.GCDetails
    { gcdetails_gen                       = 0
    , gcdetails_threads                   = 0
    , gcdetails_allocated_bytes           = 0
    , gcdetails_live_bytes                = 0
    , gcdetails_large_objects_bytes       = 0
    , gcdetails_compact_bytes             = 0
    , gcdetails_slop_bytes                = 0
    , gcdetails_mem_in_use_bytes          = 0
    , gcdetails_copied_bytes              = 0
    , gcdetails_par_max_copied_bytes      = 0
# if MIN_VERSION_base(4,11,0)
    , gcdetails_par_balanced_copied_bytes = 0
# endif
    , gcdetails_sync_elapsed_ns           = 0
    , gcdetails_cpu_ns                    = 0
    , gcdetails_elapsed_ns                = 0
    }
#else
-- | Get GC statistics.
getGcStats :: IO Stats.GCStats
# if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

-- | Empty GC statistics, as if the application hasn't started yet.
emptyGCStats :: Stats.GCStats
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    }
# else
getGcStats = Stats.getGCStats
# endif

-- | Helper to work around rename in GHC.Stats in base-4.6.
gcParTotBytesCopied :: Stats.GCStats -> Int64
# if MIN_VERSION_base(4,6,0)
gcParTotBytesCopied = Stats.parTotBytesCopied
# else
gcParTotBytesCopied = Stats.parAvgBytesCopied
# endif
#endif

------------------------------------------------------------------------
-- * Deregistering metrics

-- | An action that deregisters metrics from a metric store.
newtype Deregistration = Deregistration (State -> State)

instance Semigroup Deregistration where
  Deregistration f <> Deregistration g = Deregistration (g . f)

-- | Atomically apply a deregistration action to a metrics store.
deregister
  :: Store -- ^ Metric store
  -> Deregistration -- ^ Deregistration action
  -> IO ()
deregister (Store stateRef) (Deregistration f) =
    atomicModifyIORef' stateRef $ \state -> (f state, ())

-- | Deregister a metric (of any type).
deregisterMetric
  :: Identifier -- ^ Metric identifier
  -> Deregistration
deregisterMetric identifier =
  Deregistration $ Internal.deregister identifier

-- | Deregister all metrics (of any type) with the given name, that is,
-- irrespective of their tags.
deregisterByName
  :: T.Text -- ^ Metric name
  -> Deregistration
deregisterByName name = Deregistration $ Internal.deregisterByName name

------------------------------------------------------------------------
-- * Sampling metrics

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO Sample
sampleAll (Store store) = readIORef store >>= Internal.sampleAll
