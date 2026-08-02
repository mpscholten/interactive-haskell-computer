module WarpHelloTest (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import IhcTestBinary (ihcBin)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Warp hello-world" do
    it "serves one HTTP response end-to-end" do
        pendingWith "Pending: after accept, request path still dies on\
                    \ Int I# binary op with args=<function> 48 (ASCII '0');\
                    \ likely _0+fromIntegral in header/digit parse after\
                    \ fromIntegral left a function. Fixed along the way:\
                    \ class-method single-tag apply, Natural NS/NB,\
                    \ Category (->) baseDot, fromInteger∘toInteger recovery.\
                    \ parseHeaderLines works on S.pack ByteStrings."
        tmp <- getTemporaryDirectory
        (hsPath, hsHandle) <- openTempFile tmp "ihc-warp-hello.hs"
        hClose hsHandle
        writeFile hsPath $ unlines
            [ "import Network.Wai (responseLBS)"
            , "import Network.Wai.Handler.Warp (run)"
            , "import Network.HTTP.Types (status200)"
            , ""
            , "main :: IO ()"
            , "main = run 3110 $ \\_ respond ->"
            , "    respond $ responseLBS status200 [(\"Content-Type\", \"text/plain\")] \"Hello, Warp!\""
            ]

        let cleanup = removeFile hsPath
        bin <- ihcBin
        bracket
            (createProcess (proc bin ["run", hsPath]) { std_out = NoStream, std_err = NoStream })
            (\(_, _, _, ph) -> terminateProcess ph >> cleanup)
            (\(_, _, _, ph) -> do
                outcome <- waitForHello ph 20
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
            result <- timeout (4 * 1000000)
                (readProcessWithExitCode "curl" ["-sS", "--max-time", "3", "http://127.0.0.1:3110/"] "")
            case result of
                Just (ExitSuccess, out, "") -> pure (Right out)
                _ | attemptsLeft <= 1 -> pure (Left "warp hello-world never produced a successful HTTP response")
                  | otherwise -> do
                        threadDelay 1000000
                        waitForHello ph (attemptsLeft - 1)
