module Coverage (spec) where

import Control.Exception (bracket_)
import Data.List (isSuffixOf, sort)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>), takeBaseName, replaceExtension)
import System.IO

import Test.Hspec

import IHC.Driver (runFile)

fixtureDir :: FilePath
fixtureDir = "test/Fixtures/Coverage"

-- | Quick-wins fixtures: one-off language-extension fixtures tracked by
-- the IHP roadmap (Tier 3 in what-is-still-needed-groovy-lobster.md).
-- Separate directory so InstanceSigs / NamedFieldPuns / StandaloneDeriving
-- regressions are easy to locate.
quickWinsDir :: FilePath
quickWinsDir = "test/Fixtures/QuickWins"

captureStdout :: IO a -> IO (a, String)
captureStdout action = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "ihc-cov.txt"
    saved <- hDuplicate stdout
    hFlush stdout
    r <- bracket_
        (hDuplicateTo h stdout >> hClose h)
        (hDuplicateTo saved stdout >> hClose saved >> hFlush stdout)
        action
    out <- readFile path
    removeFile path
    pure (r, out)

spec :: Spec
spec = do
    describe "Coverage — auto-discovered fixture suite" $
        runIO (discoverFixtures fixtureDir) >>= mapM_ addTest
    describe "QuickWins — one-off language-extension fixtures" $
        runIO (discoverFixtures quickWinsDir) >>= mapM_ addTest

discoverFixtures :: FilePath -> IO [(FilePath, Maybe FilePath)]
discoverFixtures dir = do
    present <- doesDirectoryExist dir
    if not present then pure []
    else do
        entries <- sort <$> listDirectory dir
        -- Fixtures whose @main@ intentionally throws (for the raise#
        -- end-to-end test) are excluded from auto-discovery because
        -- 'runFile' propagates the exception through the
        -- hspec test.  They're exercised by a dedicated
        -- @runExpectFail@ test in "RunFile".
        let hsFiles = filter (\e -> ".hs" `isSuffixOf` e
                                  && not ("_XFAIL.hs" `isSuffixOf` e)
                                  && e `notElem` expectedFailureFixtures) entries
        mapM (mkPair dir) hsFiles

-- | Fixtures that exit non-zero by design (so the auto-runner would
-- choke on them).  Keep this list tiny: each entry needs a dedicated
-- test verifying the expected failure mode.
expectedFailureFixtures :: [FilePath]
expectedFailureFixtures =
    [ "error_message.hs"
    ]

mkPair :: FilePath -> FilePath -> IO (FilePath, Maybe FilePath)
mkPair dir hsName = do
    let hsPath  = dir </> hsName
        outPath = replaceExtension hsPath ".out"
    exists <- doesFileExist outPath
    pure (hsPath, if exists then Just outPath else Nothing)

addTest :: (FilePath, Maybe FilePath) -> Spec
addTest (hsPath, mOutPath) =
    it fixtureName $
        if fixtureName == "wait_read_socket_stm_or_else"
            then pendingWith
                "blocking STM retry wake-up needs a transactional scheduler"
            else case mOutPath of
                Nothing -> do
                    n <- runFile hsPath
                    n `shouldBe` 0
                Just outPath -> do
                    golden <- readFile outPath
                    (n, out) <- captureStdout (runFile hsPath)
                    n   `shouldBe` 0
                    out `shouldBe` golden
  where
    fixtureName = takeBaseName hsPath
