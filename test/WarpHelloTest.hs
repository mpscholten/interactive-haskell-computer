module WarpHelloTest (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import GHC.Clock (getMonotonicTimeNSec)
import IhcTestBinary (ihcBin)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.Process
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Warp hello-world" do
    it "serves one HTTP response end-to-end" do
        bin <- ihcBin
        env0 <- getEnvironment
        let childEnv = [("IHC_NO_DAEMON", "1"), ("IHC_TIMING", "1")]
                <> filter ((`notElem` ["IHC_NO_DAEMON", "IHC_TIMING"]) . fst) env0
        started <- getMonotonicTimeNSec
        bracket
            (createProcess (proc bin
                ["run", "test/Fixtures/Coverage/warp_hello_server.hs"])
                { std_out = Inherit, std_err = Inherit, env = Just childEnv })
            (\(_, _, _, ph) -> terminateProcess ph)
            (\(_, _, _, ph) -> do
                outcome <- waitForHello ph 360
                case outcome of
                    Left msg -> expectationFailure msg
                    Right (body, status) -> do
                        finished <- getMonotonicTimeNSec
                        let elapsedSeconds = fromIntegral (finished - started) / 1e9 :: Double
                        putStrLn ("[warp-acceptance] cold first response: "
                            <> show elapsedSeconds <> "s")
                        status `shouldBe` "200"
                        body `shouldBe` "Hello, Warp!"
            )

waitForHello :: ProcessHandle -> Int -> IO (Either String (String, String))
waitForHello ph attemptsLeft = do
    mExit <- getProcessExitCode ph
    case mExit of
        Just code -> pure (Left ("ihc exited before serving hello-world: " <> show code))
        Nothing -> do
            result <- timeout (305 * 1000000)
                (readProcessWithExitCode "curl"
                    ["-sS", "--connect-timeout", "1", "--max-time", "300"
                    ,"--write-out", "\n%{http_code}"
                    ,"http://127.0.0.1:3110/"] "")
            case result of
                Just (ExitSuccess, out, "") ->
                    case reverse (lines out) of
                        status : bodyLines ->
                            pure (Right (unlinesWithoutTrailingNewline
                                (reverse bodyLines), status))
                        [] -> pure (Left "curl returned an empty response")
                _ | attemptsLeft <= 1 -> pure (Left "warp hello-world never produced a successful HTTP response")
                  | otherwise -> do
                        threadDelay 1000000
                        waitForHello ph (attemptsLeft - 1)

unlinesWithoutTrailingNewline :: [String] -> String
unlinesWithoutTrailingNewline [] = ""
unlinesWithoutTrailingNewline xs = foldr1 (\a b -> a <> "\n" <> b) xs
