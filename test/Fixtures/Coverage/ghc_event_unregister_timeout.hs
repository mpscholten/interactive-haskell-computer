-- GHC.Event.unregisterTimeout must actually cancel.
-- Warp TimeManager.cancel and Event.threadDelay's
--   takeMVar `onException` unregisterTimeout
-- both depend on this. A no-op unregister leaves the callback
-- scheduled; if that callback throwTo's TimeoutThread, the timeout
-- manager thread blocks in throwTo after accept.
import Control.Concurrent (threadDelay)
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    mgr <- getSystemTimerManager
    key <- registerTimeout mgr 50000 (putStrLn "fired" >> hFlush stdout)
    unregisterTimeout mgr key
    threadDelay 200000
    putStrLn "done"
