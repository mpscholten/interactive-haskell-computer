module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Data.List (isInfixOf, sort, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))

import IHC.Driver (runFile)

-- | Isolated cross-fixture memory probe.
--
-- Runs the first @N@ @test/Fixtures/Coverage/*.hs@ fixtures back-to-back
-- in ONE process via 'IHC.Driver.runFile' and NOTHING else.  The full
-- hspec @ihc-test@ binary constructs ~31 specs' @runIO@ blocks (warp /
-- north-star / coverage discovery) before it can run a single example —
-- ~660s of heavy, leaky construction-time work that both dominates and
-- contaminates any in-suite memory measurement (and @--match@ still pays
-- it).  This exe skips all of that so the per-'loadProgramFromSource'
-- live-set curve emitted by @IHC_MEM_DEBUG@ (the @[ihc:mem]@ lines from
-- 'IHC.Scheduler.resetPerRunGlobals') is clean and attributable.
--
-- Diagnostic + Step-4 plateau gate:
--
-- > IHC_MEM_DEBUG=1 IHC_MEM_DEBUG_EVERY=5 \
-- >   cabal run ihc-mem-probe -- 120 test/Fixtures/Coverage
--
-- A linear @live_bytes@ climb across the samples = the cross-fixture
-- retention leak; a plateau = fixed.  Each 'runFile' is wrapped in
-- 'try' so a throwing fixture doesn't truncate the curve.
-- | @IHC_MEMPROBE_SKIP@ — comma-separated substrings; any fixture whose
-- basename contains one is skipped.  Used to drop the cold
-- Data.ByteString-from-source / north-star fixtures: in the full hspec
-- run ~200 prior runFile calls warm 'globalScanCacheRegistry' so those
-- scans are cached, but an isolated cold loop pays minutes per fixture
-- on them, which stalls a gradual-retention slope.  They are not the
-- gradual cross-fixture leak (that climbs across MANY light fixtures);
-- profile them separately if needed.
splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
    (a, [])      -> [a | not (null a)]
    (a, _ : rest) -> [a | not (null a)] ++ splitOnComma rest

main :: IO ()
main = do
    args <- getArgs
    skips <- splitOnComma . fromMaybe "" <$> lookupEnv "IHC_MEMPROBE_SKIP"
    let n    = case args of
                 (a:_) | [(k, "")] <- reads a -> k
                 _                            -> 120
        path = case args of
                 (_:p:_) -> p
                 _       -> "test/Fixtures/Coverage"
        keep f = not (any (`isInfixOf` f) skips)
    isFile <- doesFileExist path
    paths <-
        if isFile
            -- Single-file REPEAT mode: run the SAME fixture n times
            -- back-to-back.  Run 1 is cold; runs 2..n hit the warm
            -- content-addressed scan cache, so they isolate per-run
            -- RETENTION (live_bytes climb = the cross-run leak) from
            -- cold-scan cost.
            then pure (replicate n path)
            -- Directory mode: first n distinct fixtures (post-skip).
            else map (path </>) . take n . filter keep . sort
                     . filter (".hs" `isSuffixOf`)
                     <$> listDirectory path
    forM_ (zip [(1 :: Int) ..] paths) $ \(i, f) -> do
        r <- try (runFile f) :: IO (Either SomeException Int)
        case r of
            Right _ -> pure ()
            Left e  -> putStrLn $
                "[memprobe] run " <> show i <> " " <> f
                <> " threw: " <> take 160 (show e)
