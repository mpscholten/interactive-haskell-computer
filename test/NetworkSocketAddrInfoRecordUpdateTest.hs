module NetworkSocketAddrInfoRecordUpdateTest (spec) where

import IhcTestBinary (ihcBin)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Network.Socket AddrInfo record update" do
    it "allows defaultHints record updates used by getAddrInfo-based bind setup" do
        tmp <- getTemporaryDirectory
        (hsPath, hsHandle) <- openTempFile tmp "ihc-network-socket-addrinfo-record-update.hs"
        hClose hsHandle
        let logPath = hsPath <> ".log"
        writeFile hsPath $ unlines
            [ "import Network.Socket"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }"
            , "    addrs <- getAddrInfo (Just hints) Nothing (Just \"3110\")"
            , "    case addrs of"
            , "        [] -> writeFile " ++ show logPath ++ " \"noaddrs\\n\""
            , "        (ai:_) -> writeFile " ++ show logPath ++ " (\"family=\" ++ show (addrFamily ai) ++ \" socktype=\" ++ show (addrSocketType ai) ++ \"\\n\")"
            ]

        bin <- ihcBin
        (rc, _out, _err) <- readProcessWithExitCode bin ["run", hsPath] ""
        rc `shouldBe` ExitSuccess
        contents <- readFile logPath
        contents `shouldContain` "family="
        contents `shouldContain` "socktype="
        removeIfExists logPath
        removeFile hsPath
      where
        removeIfExists path = do
            exists <- readProcessWithExitCode "sh" ["-c", "test -f \"$1\"", "sh", path] ""
            case exists of
                (ExitSuccess, _, _) -> removeFile path
                _ -> pure ()
