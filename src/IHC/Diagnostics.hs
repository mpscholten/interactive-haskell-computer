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
    ) where

import Data.Char (toLower)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)

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
