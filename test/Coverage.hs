module Coverage (spec) where

import Control.Exception (bracket_)
import Data.List (isSuffixOf, sort)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (listDirectory, doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>), takeBaseName, replaceExtension)
import System.IO

import Test.Hspec

import IHC.Driver (runFile)

fixtureDir :: FilePath
fixtureDir = "test/Fixtures/Coverage"

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
spec = describe "Coverage — auto-discovered fixture suite" $
    runIO (discoverFixtures fixtureDir) >>= mapM_ addTest

discoverFixtures :: FilePath -> IO [(FilePath, Maybe FilePath)]
discoverFixtures dir = do
    entries <- sort <$> listDirectory dir
    let hsFiles = filter (\e -> ".hs" `isSuffixOf` e
                              && not ("_XFAIL.hs" `isSuffixOf` e)) entries
    mapM (mkPair dir) hsFiles

mkPair :: FilePath -> FilePath -> IO (FilePath, Maybe FilePath)
mkPair dir hsName = do
    let hsPath  = dir </> hsName
        outPath = replaceExtension hsPath ".out"
    exists <- doesFileExist outPath
    pure (hsPath, if exists then Just outPath else Nothing)

addTest :: (FilePath, Maybe FilePath) -> Spec
addTest (hsPath, mOutPath) =
    it (takeBaseName hsPath) $
        case mOutPath of
            Nothing -> do
                n <- runFile hsPath
                n `shouldBe` 0
            Just outPath -> do
                golden <- readFile outPath
                (n, out) <- captureStdout (runFile hsPath)
                n   `shouldBe` 0
                out `shouldBe` golden
