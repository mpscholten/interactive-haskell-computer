-- MVar put/take roundtrip on the same MVar twice, plus newMVar with
-- an initial value. Complements concurrency_forkio_mvar which also
-- spawns a fork.
import Control.Concurrent.MVar

main :: IO ()
main = do
    m <- newEmptyMVar
    putMVar m (42 :: Int)
    v <- takeMVar m
    print v
    -- reuse the same mvar
    putMVar m 100
    v2 <- takeMVar m
    print v2
    -- newMVar with initial value + readMVar
    n <- newMVar ("hello" :: String)
    s <- readMVar n
    putStrLn s
