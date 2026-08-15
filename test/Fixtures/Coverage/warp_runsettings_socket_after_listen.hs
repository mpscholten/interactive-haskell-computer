-- Warp.run leftover AFTER bind+listen: runSettingsSocket then close
-- the listen socket (same RST window as curl).  Combo2 leftover was
--   PatternMatchFail case (#,#) new_s a vs <function>
-- caught by acceptLoop's E.try as IOException, so Warp.run returned
-- (exit 0, empty stderr) and curl got connection-reset.
-- That is bindIO / thenIO (`case m s of (# new_s, a #)`) seeing a
-- leftover State# VFun.  bindPortTCP + settingsAccept are GREEN.
-- Close from a helper thread so accept is in-flight (close-before-run
-- hung on leftover accept/isErrno retry).
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import Data.Streaming.Network (bindPortTCP)
import Network.HTTP.Types (status200)
import Network.Socket (close)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "start" >> hFlush stdout
    sock <- bindPortTCP 18821 "*4"
    putStrLn "listen-ok" >> hFlush stdout
    _ <- forkIO $ do
        threadDelay 400000
        putStrLn "closing" >> hFlush stdout
        close sock
    r <- try $ runSettingsSocket defaultSettings sock $ \_ respond ->
            respond $ responseLBS status200 [] "Hello, Warp!"
    -- Canary for THIS leftover only.  Combo2 printed thenio-pmf
    -- (acceptLoop try swallowed it, curl RST).  After the bindIO
    -- VFun apply, that signature is gone; later leftovers (Size Eq)
    -- still fail the socket run but must not revive (#,#) new_s.
    case r of
        Left e
            | "new_s" `isInfixOf` show e -> putStrLn "thenio-pmf"
            | otherwise -> putStrLn "thenio-gone"
        Right _ -> putStrLn "thenio-gone"
    putStrLn "done"
