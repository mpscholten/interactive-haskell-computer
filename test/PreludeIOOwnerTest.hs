module PreludeIOOwnerTest (spec) where

import Control.Exception (bracket)
import IhcTestBinary (ihcBin)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "implicit Prelude IO source ownership" do
    it "resolves putStrLn from source in a fresh ihc process" do
        bin <- ihcBin
        withTempProgram "main = putStrLn \"fresh-hello\"\n" $ \path -> do
            (rc, out, err) <- readProcessWithExitCode bin ["run", path] ""
            rc `shouldBe` ExitSuccess
            err `shouldBe` ""
            out `shouldBe` "fresh-hello\n"

    it "resolves print from the same public System.IO surface" do
        bin <- ihcBin
        withTempProgram "main = print (42 :: Int)\n" $ \path -> do
            (rc, out, err) <- readProcessWithExitCode bin ["run", path] ""
            rc `shouldBe` ExitSuccess
            err `shouldBe` ""
            out `shouldBe` "42\n"

withTempProgram :: String -> (FilePath -> IO a) -> IO a
withTempProgram source = bracket create removeFile
  where
    create = do
        tmp <- getTemporaryDirectory
        (path, h) <- openTempFile tmp "ihc-prelude-io-owner.hs"
        hPutStr h source
        hClose h
        pure path
