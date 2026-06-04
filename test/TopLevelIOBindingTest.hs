module TopLevelIOBindingTest (spec) where

import IhcTestBinary (ihcBin)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

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
        bin <- ihcBin
        (_rc, _out, _err) <- readProcessWithExitCode bin ["run", hsPath] ""
        exists <- doesFileExist marker
        exists `shouldBe` True
        whenExistsRemove marker
        removeFile hsPath

whenExistsRemove :: FilePath -> IO ()
whenExistsRemove path = do
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
