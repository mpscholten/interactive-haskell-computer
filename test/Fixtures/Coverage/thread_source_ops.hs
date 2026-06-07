import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import GHC.Conc.Sync (fromThreadId)

main :: IO ()
main = do
    tid <- myThreadId
    fromThreadId tid `seq` putStrLn "myThreadId-source"

    m <- newEmptyMVar
    child <- forkIO $ do
        threadDelay 1000000
        putMVar m "missed"
    killThread child
    threadDelay 50000
    killed <- tryTakeMVar m
    case killed of
        Nothing -> putStrLn "killThread-source"
        Just _  -> putStrLn "killThread-missed"

    self <- myThreadId
    r <- try (throwTo self (userError "self boom")) :: IO (Either SomeException ())
    case r of
        Left _  -> putStrLn "throwTo-source"
        Right _ -> putStrLn "throwTo-missed"
