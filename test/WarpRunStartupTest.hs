module WarpRunStartupTest (spec) where

import IhcTestBinary (ihcBin)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Warp run startup" do
    it "runs preceding IO before executing runSettings when the action is only aliased" do
        tmp <- getTemporaryDirectory
        (hsPath, hsHandle) <- openTempFile tmp "ihc-warp-run-startup.hs"
        hClose hsHandle
        let marker = hsPath <> ".marker"
        writeFile hsPath $ unlines
            [ "import Network.Wai (Application)"
            , "import Network.Wai.Handler.Warp (runSettings)"
            , ""
            , "app :: Application"
            , "app _ _ = error \"unused\""
            , ""
            , "server :: IO ()"
            , "server = runSettings undefined app"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    writeFile " ++ show marker ++ " \"start\\n\""
            , "    let _unused = server"
            , "    appendFile " ++ show marker ++ " \"after\\n\""
            ]

        bin <- ihcBin
        (_rc, _out, _err) <- readProcessWithExitCode bin ["run", hsPath] ""
        exists <- doesFileExist marker
        exists `shouldBe` True
        markerContents <- readFile marker
        markerContents `shouldBe` "start\nafter\n"
        whenExistsRemove marker
        removeFile hsPath

whenExistsRemove :: FilePath -> IO ()
whenExistsRemove path = do
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
