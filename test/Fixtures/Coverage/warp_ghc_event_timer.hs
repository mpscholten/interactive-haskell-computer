-- GHC.Event timer smoke test: registerTimeout schedules a callback that
-- fires before the main thread's threadDelay returns.  Locks in the
-- minimum-viable contract that warp's connection time-manager relies
-- on (slowloris timeouts, idle connection sweep).
import GHC.Event (getSystemTimerManager, registerTimeout)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    mgr <- getSystemTimerManager
    _   <- registerTimeout mgr 50000 (putStrLn "fired")
    threadDelay 200000
    putStrLn "done"
