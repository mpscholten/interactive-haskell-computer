-- | Lightweight diagnostic helpers for IHC.
--
-- The interpreter has several places where it falls back silently to a
-- degraded mode (empty stub module when an import cannot be located,
-- single-file search path when a Cabal project is missing a freeze
-- file, etc.). Those fallbacks are legitimate for the happy path but
-- catastrophic for debugging: a missing import cascades into a storm of
-- 'UnresolvedName' errors that look nothing like the original cause.
--
-- This module exposes a single entry point, 'warnStub', that the
-- fallback callsites use to emit a tagged @[ihc:warn] …@ line on
-- stderr before returning the stub/fallback value. The warnings can be
-- silenced by setting the @IHC_WARN_STUBS=0@ environment variable (any
-- of @0@ / @false@ / @no@ / @off@, case-insensitive).
--
-- Warnings are emitted to stderr only; stdout stays clean for REPL
-- output and program output.
module IHC.Diagnostics
    ( warnStub
    , warnStubOnce
    , warnStubsEnabled
    , traceLine
    , traceEnabled
    , memDebugEnabled
    , memDebugEvery
    , evalStatsEnabled
    , evalStatsEvery
    , noteForceEval
    , noteForceKind
    , noteBlackHoleWait
    ) where

import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word64)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | 'True' iff the @IHC_WARN_STUBS@ env var is not set to a disabling
-- value. Default (unset) is warnings-on. The gate is re-read on every
-- call so tests can flip it without restarting the process.
warnStubsEnabled :: IO Bool
warnStubsEnabled = do
    mv <- lookupEnv "IHC_WARN_STUBS"
    pure $ case mv of
        Nothing -> True
        Just v  -> case map toLower v of
            "0"     -> False
            "false" -> False
            "no"    -> False
            "off"   -> False
            ""      -> True
            _       -> True

-- | Emit a @[ihc:warn] …@ line on stderr, unless the @IHC_WARN_STUBS@
-- env var is set to a disabling value. Flushes stderr so the warning
-- isn't lost if we crash immediately afterwards.
warnStub :: String -> IO ()
warnStub msg = do
    enabled <- warnStubsEnabled
    if enabled
        then emitWarn msg
        else pure ()

-- | Like 'warnStub', but suppresses duplicate messages in the current
-- process. Use this for expected per-run fallbacks where repeated
-- identical lines drown out fixture output.
warnStubOnce :: String -> IO ()
warnStubOnce msg = do
    enabled <- warnStubsEnabled
    if enabled
        then do
            shouldEmit <- atomicModifyIORef' warnedOnceRef $ \seen ->
                if msg `elem` seen
                    then (seen, False)
                    else (msg : seen, True)
            if shouldEmit
                then emitWarn msg
                else pure ()
        else pure ()

emitWarn :: String -> IO ()
emitWarn msg = do
    hPutStrLn stderr ("[ihc:warn] " <> msg)
    hFlush stderr

warnedOnceRef :: IORef [String]
warnedOnceRef = unsafePerformIO (newIORef [])
{-# NOINLINE warnedOnceRef #-}

-- | 'True' iff the @IHC_TRACE@ env var is set to an enabling value
-- (@1@ / @true@ / @yes@ / @on@, case-insensitive). Default (unset) is
-- tracing-off. Unlike 'warnStubsEnabled' the default here is OFF because
-- trace output is high-volume and meant for debugging hangs only.
--
-- The lookup is cached in a top-level CAF so that tight instrumentation
-- loops don't hit 'lookupEnv' on every iteration.
traceEnabled :: Bool
traceEnabled = unsafePerformIO $ do
    mv <- lookupEnv "IHC_TRACE"
    pure $ case mv of
        Nothing -> False
        Just v  -> case map toLower v of
            "1"     -> True
            "true"  -> True
            "yes"   -> True
            "on"    -> True
            _       -> False
{-# NOINLINE traceEnabled #-}

-- | Emit a @[ihc:trace] …@ line on stderr when 'IHC_TRACE' is enabled.
-- Used as a heartbeat in long-running load paths (e.g. the
-- @runUntilStable@ fixpoint loop, module loads, and preloading) so
-- users can see what the interpreter is doing instead of staring at a
-- silent hang.
--
-- Completely silent when 'IHC_TRACE' is unset or set to a disabling
-- value. Flushes stderr on every call so the last trace line before a
-- crash or infinite loop is visible.
traceLine :: String -> IO ()
traceLine msg =
    if traceEnabled
        then do
            hPutStrLn stderr ("[ihc:trace] " <> msg)
            hFlush stderr
        else pure ()

-- | 'True' iff the @IHC_MEM_DEBUG@ env var is set to an enabling value
-- (@1@ / @true@ / @yes@ / @on@, case-insensitive). Default (unset) is
-- OFF. Cached in a top-level CAF — identical disposition to
-- 'traceEnabled' — so the per-fixture guard in
-- 'IHC.Scheduler.resetPerRunGlobals' is a single boolean test and the
-- whole 'IHC.MemDebug' machinery is zero-cost in normal/CI runs.
memDebugEnabled :: Bool
memDebugEnabled = unsafePerformIO $ do
    mv <- lookupEnv "IHC_MEM_DEBUG"
    pure $ case mv of
        Nothing -> False
        Just v  -> case map toLower v of
            "1"     -> True
            "true"  -> True
            "yes"   -> True
            "on"    -> True
            _       -> False
{-# NOINLINE memDebugEnabled #-}

-- | How many fixtures between @[ihc:mem]@ dumps.  Read from
-- @IHC_MEM_DEBUG_EVERY@ (default 25); a non-positive or unparseable
-- value falls back to 25.  Cached CAF, only consulted when
-- 'memDebugEnabled'.
memDebugEvery :: Int
memDebugEvery = unsafePerformIO $ do
    mv <- lookupEnv "IHC_MEM_DEBUG_EVERY"
    pure $ case mv >>= readMaybe of
        Just n | n > 0 -> n
        _              -> 25
{-# NOINLINE memDebugEvery #-}

--------------------------------------------------------------------------------
-- Eval hot-path sampling (post-recv busy-spin diagnosis)
--------------------------------------------------------------------------------

-- | 'True' iff @IHC_EVAL_STATS@ is set to an enabling value
-- (@1@ / @true@ / @yes@ / @on@). Default OFF. Cached CAF so the
-- 'force' hot path pays a single boolean test when disabled.
evalStatsEnabled :: Bool
evalStatsEnabled = unsafePerformIO $ do
    mv <- lookupEnv "IHC_EVAL_STATS"
    pure $ case mv of
        Nothing -> False
        Just v  -> case map toLower v of
            "1"     -> True
            "true"  -> True
            "yes"   -> True
            "on"    -> True
            _       -> False
{-# NOINLINE evalStatsEnabled #-}

-- | How many 'force' entries between sample lines.  Read from
-- @IHC_EVAL_STATS_EVERY@ (default 200000). Only consulted when
-- 'evalStatsEnabled'.
evalStatsEvery :: Word64
evalStatsEvery = unsafePerformIO $ do
    mv <- lookupEnv "IHC_EVAL_STATS_EVERY"
    pure $ case mv >>= readMaybe of
        Just n | n > 0 -> fromIntegral (n :: Integer)
        _              -> 200000
{-# NOINLINE evalStatsEvery #-}

forceSampleCounter :: IORef Word64
forceSampleCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE forceSampleCounter #-}

bhWaitCounter :: IORef Word64
bhWaitCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE bhWaitCounter #-}

-- | Record one 'force' entry of any kind. @kind@ is a short tag
-- (@eval@ / @whnf@ / @bh@ / @lazy@ / @apply@ …); @sample@ is optional
-- context (expr show, method name, …). Every 'evalStatsEvery' forces
-- emit a @[ihc:eval]@ line so a busy-spin shows which path is hot.
noteForceEval :: String -> IO ()
noteForceEval sample = noteForceKind "eval" sample

-- | Like 'noteForceEval' but with an explicit kind tag for non-Unevaluated
-- force paths (already-memoised WHNF, black-hole waits, lazy builtins).
noteForceKind :: String -> String -> IO ()
noteForceKind kind sample =
    if not evalStatsEnabled
        then pure ()
        else do
            n <- atomicModifyIORef' forceSampleCounter $ \c ->
                let c' = c + 1 in (c', c')
            let every = evalStatsEvery
            if n `mod` every == 0
                then do
                    bh <- atomicModifyIORef' bhWaitCounter $ \c -> (c, c)
                    hPutStrLn stderr $
                        "[ihc:eval] force#" <> show n
                          <> " kind=" <> kind
                          <> " bh_waits=" <> show bh
                          <> " sample=" <> take 240 sample
                    hFlush stderr
                else pure ()

-- | Count foreign black-hole yield-waits and sample them as force traffic
-- so a pure BH-yield spin still shows up in @[ihc:eval]@ lines.
noteBlackHoleWait :: IO ()
noteBlackHoleWait =
    if not evalStatsEnabled
        then pure ()
        else do
            atomicModifyIORef' bhWaitCounter $ \c -> (c + 1, ())
            noteForceKind "bh" "<foreign-blackhole-yield>"
