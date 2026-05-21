module NetworkSocketAddrInfoRecordUpdateTest (spec) where

import Control.Exception (finally)
import Data.List (isInfixOf)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

spec :: Spec
spec = do
    describe "generic imported record syntax" do
        it "keeps imported direct positional constructor application working" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import A"
                    , ""
                    , "main :: IO ()"
                    , "main = print (UniqueR 1 2)"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on direct imported positional constructor application"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "keeps imported let-bound positional constructor application working" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import A"
                    , ""
                    , "main :: IO ()"
                    , "main = let x = UniqueR 1 2 in print x"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on let-bound imported positional constructor application"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "keeps imported manual case-based update working" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import A"
                    , ""
                    , "main :: IO ()"
                    , "main = case u0 of"
                    , "    UniqueR a b -> print (UniqueR 3 b)"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on manual imported case-based update"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 3 1"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "keeps local record construction working in the tiny module" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "module Main where"
                    , ""
                    , "data LocalR = LocalR { lf :: Int, lg :: Int } deriving Show"
                    , ""
                    , "main :: IO ()"
                    , "main = print (LocalR { lf = 1, lg = 2 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on local record construction"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "LocalR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "keeps local record update working in the tiny module" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "module Main where"
                    , ""
                    , "data LocalR = LocalR { lf :: Int, lg :: Int } deriving Show"
                    , ""
                    , "r0 :: LocalR"
                    , "r0 = LocalR 0 1"
                    , ""
                    , "main :: IO ()"
                    , "main = print (r0 { lf = 3 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on local record update"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "LocalR 3 1"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on imported unqualified record construction even though the positional form works" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import A"
                    , ""
                    , "main :: IO ()"
                    , "main = let x = UniqueR { uf = 1, ug = 2 } in print x"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on imported unqualified record construction"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on imported unqualified record update" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import A"
                    , ""
                    , "main :: IO ()"
                    , "main = print (u0 { uf = 3 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on imported unqualified record update"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 3 1"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on qualified imported record construction too" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import qualified A"
                    , ""
                    , "main :: IO ()"
                    , "main = print (A.UniqueR { A.uf = 1, A.ug = 2 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on qualified imported record construction"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on qualified imported record update too" do
            withTinyImportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import qualified A"
                    , ""
                    , "main :: IO ()"
                    , "main = print (A.u0 { A.uf = 3 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on qualified imported record update"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 3 1"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "keeps re-exported positional constructor application working" do
            withTinyReexportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import B"
                    , ""
                    , "main :: IO ()"
                    , "main = print (UniqueR 1 2)"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on re-exported positional constructor application"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on re-exported imported record construction too" do
            withTinyReexportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import B"
                    , ""
                    , "main :: IO ()"
                    , "main = let x = UniqueR { uf = 1, ug = 2 } in print x"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on re-exported imported record construction"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 1 2"
                        err `shouldSatisfy` (not . hasRecordDebug)

        it "hangs today on re-exported imported record update too" do
            withTinyReexportedRecordModule $ \mainPath -> do
                writeFile mainPath $ unlines
                    [ "import B"
                    , ""
                    , "main :: IO ()"
                    , "main = print (u0 { uf = 3 })"
                    ]
                result <- runIhc mainPath
                case result of
                    Nothing -> expectationFailure "ihc hung on re-exported imported record update"
                    Just (rc, out, err) -> do
                        rc `shouldBe` ExitSuccess
                        out `shouldContain` "UniqueR 3 1"
                        err `shouldSatisfy` (not . hasRecordDebug)

    describe "Network.Socket AddrInfo record update" do
        it "evaluates a minimal defaultHints record update from Network.Socket.Info" do
            tmp <- getTemporaryDirectory
            (hsPath, hsHandle) <- openTempFile tmp "ihc-network-socket-info-addrinfo-record-update.hs"
            hClose hsHandle
            writeFile hsPath $ unlines
                [ "import Network.Socket.Info"
                , ""
                , "main :: IO ()"
                , "main = do"
                , "    let hints = defaultHints { addrFlags = [] }"
                , "    print (addrFamily hints)"
                ]

            result <- runIhc hsPath
            case result of
                Nothing ->
                    expectationFailure "ihc hung evaluating a minimal AddrInfo record update"
                Just (rc, out, err) -> do
                    rc `shouldBe` ExitSuccess
                    out `shouldContain` "Family"
                    err `shouldSatisfy` (not . hasRecordDebug)

            removeFile hsPath

    it "allows defaultHints record updates used by getAddrInfo-based bind setup" do
        pendingWith "Pending: focused minimal record-update regression above still fails; re-enable this broader getAddrInfo coverage after the underlying AddrInfo record syntax bug is fixed."

runIhc :: FilePath -> IO (Maybe (ExitCode, String, String))
runIhc hsPath = timeout (10 * 1000000) (readProcessWithExitCode ihcBin ["run", hsPath] "")

hasRecordDebug :: String -> Bool
hasRecordDebug = isInfixOf "[ihc:record]"

withTinyImportedRecordModule :: (FilePath -> IO a) -> IO a
withTinyImportedRecordModule action = do
    tmp <- getTemporaryDirectory
    (tmpPath, tmpHandle) <- openTempFile tmp "ihc-imported-record"
    hClose tmpHandle
    removeFile tmpPath
    createDirectory tmpPath
    let aPath = tmpPath <> "/A.hs"
        mainPath = tmpPath <> "/Main.hs"
    writeFile aPath $ unlines
        [ "module A (UniqueR(..), u0) where"
        , ""
        , "data UniqueR = UniqueR { uf :: Int, ug :: Int } deriving Show"
        , "u0 :: UniqueR"
        , "u0 = UniqueR 0 1"
        ]
    action mainPath `finally` removeDirectoryRecursive tmpPath

withTinyReexportedRecordModule :: (FilePath -> IO a) -> IO a
withTinyReexportedRecordModule action = do
    tmp <- getTemporaryDirectory
    (tmpPath, tmpHandle) <- openTempFile tmp "ihc-reexported-record"
    hClose tmpHandle
    removeFile tmpPath
    createDirectory tmpPath
    let aPath = tmpPath <> "/A.hs"
        bPath = tmpPath <> "/B.hs"
        mainPath = tmpPath <> "/Main.hs"
    writeFile aPath $ unlines
        [ "module A (UniqueR(..), u0) where"
        , ""
        , "data UniqueR = UniqueR { uf :: Int, ug :: Int } deriving Show"
        , "u0 :: UniqueR"
        , "u0 = UniqueR 0 1"
        ]
    writeFile bPath $ unlines
        [ "module B (module A) where"
        , "import A"
        ]
    action mainPath `finally` removeDirectoryRecursive tmpPath
