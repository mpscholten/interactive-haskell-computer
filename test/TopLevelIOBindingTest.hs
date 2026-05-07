module TopLevelIOBindingTest (spec) where

import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

spec :: Spec
spec = describe "top-level IO binding" do
    it "runs preceding statements before a referenced top-level IO action" do
        tmp <- getTemporaryDirectory
        (hsPath, hsHandle) <- openTempFile tmp "ihc-top-io-binding.hs"
        hClose hsHandle
        let marker = hsPath <> ".marker"
        writeFile hsPath $ unlines
            [ "server :: IO ()"
            , "server = appendFile " ++ show marker ++ " \"server\\n\""
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    writeFile " ++ show marker ++ " \"start\\n\""
            , "    server"
            ]
        (_rc, _out, _err) <- readProcessWithExitCode ihcBin ["run", hsPath] ""
        exists <- doesFileExist marker
        exists `shouldBe` True
        whenExistsRemove marker
        removeFile hsPath

whenExistsRemove :: FilePath -> IO ()
whenExistsRemove path = do
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
