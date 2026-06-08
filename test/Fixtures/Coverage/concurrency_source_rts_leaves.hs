import Control.Concurrent (myThreadId, threadDelay)
import qualified GHC.Conc.Sync as Sync

main :: IO ()
main = do
    tid <- myThreadId
    Sync.fromThreadId tid `seq` putStrLn "fromThreadId-source"
    caps <- Sync.getNumCapabilities
    if caps >= 1
        then putStrLn "capabilities-source"
        else putStrLn "capabilities-bad"
    threadDelay 1000
    putStrLn "threadDelay-source"
