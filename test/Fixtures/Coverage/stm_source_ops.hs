import Control.Concurrent.STM

main :: IO ()
main = do
    t <- atomically (newTVar (10 :: Int))
    print =<< readTVarIO t
    atomically (writeTVar t 20)
    print =<< readTVarIO t
    atomically (modifyTVar t (+ 1))
    print =<< readTVarIO t
    atomically (modifyTVar' t (* 2))
    print =<< readTVarIO t
    r <- atomically (retry `orElse` readTVar t)
    print r
    atomically (check (r == 42))
    print (t == t)
    u <- newTVarIO (42 :: Int)
    print (t == u)
