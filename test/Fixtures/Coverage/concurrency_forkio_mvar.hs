import Control.Concurrent
import Control.Concurrent.MVar

main = do
    mvar <- newEmptyMVar
    forkIO $ do
        putMVar mvar 42
    val <- takeMVar mvar
    print (val :: Int)
    -- Simple MVar as mutex
    counter <- newMVar (0 :: Int)
    modifyMVar_ counter (\n -> pure (n + 1))
    modifyMVar_ counter (\n -> pure (n + 1))
    v <- readMVar counter
    print v
