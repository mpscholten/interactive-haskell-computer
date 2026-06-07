import Control.Concurrent.MVar

main :: IO ()
main = do
    m <- newEmptyMVar
    print =<< isEmptyMVar m
    print =<< tryPutMVar m (10 :: Int)
    print =<< tryPutMVar m 20
    print =<< isEmptyMVar m
    old <- swapMVar m 30
    print old
    readMVar m >>= print
    withMVar m (\n -> pure (n + 1)) >>= print
    modifyMVar m (\n -> pure (n + 2, n + 3)) >>= print
    readMVar m >>= print
    tryTakeMVar m >>= print
    tryTakeMVar m >>= print
