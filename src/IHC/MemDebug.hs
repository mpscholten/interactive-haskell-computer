-- | Flag-gated cross-fixture memory diagnostics (@IHC_MEM_DEBUG@).
--
-- The full @ihc-test@ suite runs ~354 fixtures in ONE hspec process.
-- 'IHC.Scheduler.resetPerRunGlobals' wipes the per-run root refs at the
-- start of every fixture, but a live closure pinning a prior run's
-- module\/Env\/Thunk graph defeats that (the @c921fb4@ bug class) and
-- the live set climbs until the @-M@ heap cap is hit (the master-CI
-- OOM this instrumentation exists to diagnose).  Every Nth fixture
-- 'dumpMemStats' prints post-major-GC live bytes plus the size of each
-- suspected retainer, so a linear climb names the leak without a
-- 45-minute full-suite run.
--
-- Leaf module on purpose: it imports the low-level state modules
-- ('IHC.Source' \/ 'IHC.FFI' \/ 'IHC.PatSyn') and is imported by
-- 'IHC.Scheduler'.  Routing this through 'IHC.Diagnostics' (which is
-- base-only and imported very widely) would create an import cycle.
--
-- Zero-cost when @IHC_MEM_DEBUG@ is unset: callers guard every use
-- with 'IHC.Diagnostics.memDebugEnabled' (a cached CAF boolean) and
-- this module is never entered; 'dumpMemStats' re-checks the flag so
-- it is also safe if ever called unguarded.
module IHC.MemDebug
    ( dumpMemStats
    ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import GHC.Stats
    ( getRTSStats, getRTSStatsEnabled
    , RTSStats(gc, max_live_bytes), GCDetails(gcdetails_live_bytes)
    )
import System.IO (hFlush, hPutStrLn, stderr)
import System.Mem (performMajorGC)

import IHC.Diagnostics (memDebugEnabled)
import IHC.FFI (openLibs, symbolCache)
import IHC.PatSyn (globalPatSynRef)
import IHC.Source (globalScanCacheRegistry)

-- | Emit one @[ihc:mem] …@ stderr line: post-major-GC live bytes plus
-- the live size of every suspected cross-fixture retainer.  The three
-- 'Int' arguments are Scheduler-private ref sizes
-- (@globalLoadedModulesRef@, @envFallbackCache@,
-- @globalEarlyBuiltinsRef@) passed in by the caller so this module
-- need not widen 'IHC.Scheduler''s export surface — same discipline as
-- the @clear*@ reset helpers.  Forces a major GC first so the live
-- figure reflects genuinely-retained heap, not float.  The line format
-- mirrors the existing @[ihc:discover] total=@ heartbeat so the two
-- correlate in CI logs.
dumpMemStats :: String -> Int -> Int -> Int -> IO ()
dumpMemStats label loadedMods envFbCache earlyBuiltins =
    when memDebugEnabled $ do
        performMajorGC
        statsOn <- getRTSStatsEnabled
        liveStr <-
            if statsOn
                then do
                    s <- getRTSStats
                    pure $ show (gcdetails_live_bytes (gc s))
                        <> " (max " <> show (max_live_bytes s) <> ")"
                else pure "n/a (RTS run without -T)"
        outer     <- readIORef globalScanCacheRegistry
        scanInner <- sum <$> mapM (fmap Map.size . readIORef) (Map.elems outer)
        nLibs <- length  <$> readIORef openLibs
        nSym  <- Map.size <$> readIORef symbolCache
        nPat  <- Map.size <$> readIORef globalPatSynRef
        hPutStrLn stderr $
            "[ihc:mem] " <> label
            <> " | live_bytes=" <> liveStr
            <> " | scanReg_outer=" <> show (Map.size outer)
            <> " scanReg_inner=" <> show scanInner
            <> " | openLibs=" <> show nLibs
            <> " symCache=" <> show nSym
            <> " patSyn=" <> show nPat
            <> " | loadedMods=" <> show loadedMods
            <> " envFbCache=" <> show envFbCache
            <> " earlyBuiltins=" <> show earlyBuiltins
        hFlush stderr
