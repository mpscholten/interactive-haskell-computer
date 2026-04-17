-- | North-star regression: run the upstream bytestring test suite
-- (@bytestring-0.12.2.0/tests/Main.hs@) under the ihc interpreter.
--
-- This is Phase 2.13's "done" signal for the interpreter: when this
-- test passes we've crossed the north-star — ihc is running a
-- nontrivial Hackage test binary (tasty + QuickCheck + the full
-- bytestring API) end-to-end from source.
--
-- __Why pending by default.__ Today the run hangs (see
-- @/Users/marc/.claude/plans/bytestring-northstar-probe-2.md@), so we
-- gate it behind the @IHC_NORTHSTAR=1@ env var.  That keeps the
-- default @cabal test@ run fast while still giving future us a single
-- command to flip and watch progress.  The day it times out with more
-- of the tasty summary in the captured stdout is the day we are
-- closer; the day it exits 0 with @"All N tests passed"@ is the day
-- we've landed the north-star.
--
-- __How to enable.__
--
-- @
--   IHC_NORTHSTAR=1 cabal test ihc-test --test-options='--match NorthStar'
--   -- optional: bump the 30s default
--   IHC_NORTHSTAR=1 IHC_NORTHSTAR_TIMEOUT=120 cabal test ihc-test ...
-- @
--
-- __Prerequisite.__ The bytestring sources must be cached at
-- @~/.cache/ihc/sources/bytestring-0.12.2.0/@ (see
-- @scripts/cache-test-deps.sh@).  If they aren't, the test marks
-- itself pending with an explanatory message.
module NorthStarTest (spec) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)

-- | Path to the built ihc binary — matches ReplTest.ihcBin.
ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

-- | Default 30-second timeout.  Dial up with @IHC_NORTHSTAR_TIMEOUT=N@.
defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 30

-- | Read @IHC_NORTHSTAR_TIMEOUT@ if set and valid, otherwise fall back.
resolveTimeoutSeconds :: IO Int
resolveTimeoutSeconds = do
    mEnv <- lookupEnv "IHC_NORTHSTAR_TIMEOUT"
    pure $ case mEnv >>= readMaybe of
        Just n | n > 0 -> n
        _              -> defaultTimeoutSeconds

-- | Return the last N lines of a string as a single newline-joined
-- string.  Used to quote a progress trailer in the timeout message so a
-- future maintainer can tell at a glance whether ihc got further than
-- the previous run.
tailLines :: Int -> String -> String
tailLines n s =
    let ls = lines s
        keep = drop (max 0 (length ls - n)) ls
    in unlines keep

spec :: Spec
spec = describe "NorthStar (Phase 2.13)" do
    it "runs bytestring-0.12.2.0/tests/Main.hs under ihc (pending until fixed)" do
        enabled <- lookupEnv "IHC_NORTHSTAR"
        case enabled of
            Just "1" -> runIt
            _        -> pendingWith
                "set IHC_NORTHSTAR=1 to run; hangs today (Phase 2.13 north-star regression gate)"
  where
    runIt = do
        home <- getHomeDirectory
        let mainHs = home
                  <> "/.cache/ihc/sources/bytestring-0.12.2.0/tests/Main.hs"
        exists <- doesFileExist mainHs
        if not exists
            then pendingWith
                ("bytestring source not cached at " <> mainHs
                 <> " — run scripts/cache-test-deps.sh")
            else do
                timeoutS <- resolveTimeoutSeconds
                result   <- timeout (timeoutS * 1000000)
                             (readProcessWithExitCode ihcBin ["run", mainHs] "")
                case result of
                    Nothing ->
                        -- No captured output available when timeout fires
                        -- via System.Timeout because readProcessWithExitCode
                        -- won't have returned yet; surface the hang as a
                        -- clear, descriptive failure.  Future iteration may
                        -- capture stdout incrementally instead.
                        expectationFailure $ unlines
                            [ "bytestring test suite didn't complete within "
                              <> show timeoutS <> "s — progress shown below:"
                            , "  (no stdout captured; timeout fired before the child returned)"
                            , "This is the expected regression/progress signal today."
                            , "When ihc gets further, bump IHC_NORTHSTAR_TIMEOUT or investigate."
                            ]
                    Just (code, out, err) ->
                        case code of
                            ExitSuccess ->
                                out `shouldSatisfy` \s ->
                                    "tests passed" `isInfixOf` s
                            ExitFailure n ->
                                expectationFailure $ unlines
                                    [ "bytestring test suite exited with code "
                                      <> show n
                                    , "-- stdout (tail) --"
                                    , tailLines 40 out
                                    , "-- stderr (tail) --"
                                    , tailLines 40 err
                                    ]
