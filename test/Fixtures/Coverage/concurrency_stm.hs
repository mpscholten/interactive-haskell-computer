import Control.Concurrent.STM

main = do
    tvar <- newTVarIO (0 :: Int)
    -- increment 5 times atomically
    atomically $ modifyTVar tvar (+1)
    atomically $ modifyTVar tvar (+1)
    atomically $ modifyTVar tvar (+1)
    val <- readTVarIO tvar
    print val
    -- read inside atomically
    v2 <- atomically $ do
        modifyTVar tvar (*2)
        readTVar tvar
    print v2
