module FreshProcessTest (spec) where

import Control.Exception (bracket, evaluate)
import IhcTestBinary (ihcBin)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, openTempFile)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc
    , terminateProcess, waitForProcess
    )
import System.Timeout (timeout)
import Test.Hspec

-- These checks deliberately use a new interpreter process per fixture.  The
-- main RunFile suite shares source/module registries, so a passing in-process
-- test alone cannot prove that discovery works before another fixture primes
-- those registries.
spec :: Spec
spec = describe "fresh-process source discovery" do
    it "runs representative class, Template Haskell, and Blaze programs unprimed" do
        bin <- ihcBin
        runFresh bin "test/Fixtures/Coverage/class_method_competing_list_alias.hs"
            `shouldReturn` Right (ExitSuccess, "11\n")
        runFresh bin "test/Fixtures/Coverage/th_pure_quote_splice.hs"
            `shouldReturn` Right (ExitSuccess, "42\n")
        runFresh bin "examples/blaze_hello/Main.hs"
            `shouldReturn` Right (ExitSuccess, "<h1>Hello world</h1>\n")

runFresh :: FilePath -> FilePath -> IO (Either String (ExitCode, String))
runFresh bin fixture = withCaptureFiles $ \outPath outH errPath errH -> do
    (_, _, _, ph) <- createProcess (proc bin ["run", fixture])
        { std_out = UseHandle outH
        , std_err = UseHandle errH
        }
    completed <- timeout (60 * 1000000) (waitForProcess ph)
    rc <- case completed of
        Just code -> pure (Right code)
        Nothing -> do
            terminateProcess ph
            _ <- waitForProcess ph
            pure (Left ("timed out running " <> fixture))
    hClose outH
    hClose errH
    out <- readStrict outPath
    err <- readStrict errPath
    pure $ case rc of
        Left message -> Left (message <> "; stderr: " <> err)
        Right code   -> Right (code, out)

withCaptureFiles
    :: (FilePath -> Handle -> FilePath -> Handle -> IO a)
    -> IO a
withCaptureFiles action = do
    tmp <- getTemporaryDirectory
    bracket (openTempFile tmp "ihc-fresh-stdout") cleanup $ \(outPath, outH) ->
        bracket (openTempFile tmp "ihc-fresh-stderr") cleanup $ \(errPath, errH) ->
            action outPath outH errPath errH
  where
    cleanup (path, h) = do
        hClose h
        removeFile path

readStrict :: FilePath -> IO String
readStrict path = do
    contents <- readFile path
    _ <- evaluate (length contents)
    pure contents
