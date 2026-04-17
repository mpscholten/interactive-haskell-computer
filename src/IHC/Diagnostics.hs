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
    , warnStubsEnabled
    , traceLine
    , traceEnabled
    ) where

import Data.Char (toLower)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

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
        then do
            hPutStrLn stderr ("[ihc:warn] " <> msg)
            hFlush stderr
        else pure ()

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
