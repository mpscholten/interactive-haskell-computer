module NetworkSocketAddrInfoRecordUpdateTest (spec) where

import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

spec :: Spec
spec = describe "Network.Socket AddrInfo record update" do
    it "allows defaultHints record updates used by getAddrInfo-based bind setup" do
        pendingWith "Pending: AddrInfo's addrFlags/addrSocketType selectors are\
                    \ not yet plumbed into the visible field registry for record\
                    \ updates. The desugarer falls through to\
                    \ 'record update: field(s) ... not in registry'."
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

        (rc, _out, err) <- readProcessWithExitCode ihcBin ["run", hsPath] ""
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
