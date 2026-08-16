module WarpHelloTest (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import IhcTestBinary (ihcBin)
import System.Exit (ExitCode(..))
import System.Process
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Warp hello-world" do
    it "serves one HTTP response end-to-end" do
        bin <- ihcBin
        bracket
            (createProcess (proc bin
                ["run", "test/Fixtures/Coverage/warp_hello_server.hs"])
                { std_out = Inherit, std_err = Inherit })
            (\(_, _, _, ph) -> terminateProcess ph)
            (\(_, _, _, ph) -> do
                outcome <- waitForHello ph 360
                case outcome of
                    Left msg -> expectationFailure msg
                    Right body -> body `shouldBe` "Hello, Warp!"
            )

waitForHello :: ProcessHandle -> Int -> IO (Either String String)
waitForHello ph attemptsLeft = do
    mExit <- getProcessExitCode ph
    case mExit of
        Just code -> pure (Left ("ihc exited before serving hello-world: " <> show code))
        Nothing -> do
            result <- timeout (305 * 1000000)
                (readProcessWithExitCode "curl"
                    ["-sS", "--connect-timeout", "1", "--max-time", "300"
                    ,"http://127.0.0.1:3110/"] "")
            case result of
                Just (ExitSuccess, out, "") -> pure (Right out)
                _ | attemptsLeft <= 1 -> pure (Left "warp hello-world never produced a successful HTTP response")
                  | otherwise -> do
                        threadDelay 1000000
                        waitForHello ph (attemptsLeft - 1)
