import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

pingpong :: MVar () -> MVar () -> Int -> IO ()
pingpong ping pong n =
    if n <= 0
        then pure ()
        else do
            takeMVar ping
            putStrLn "ping"
            putMVar pong ()
            pingpong ping pong (n - 1)

pongthread :: MVar () -> MVar () -> Int -> IO ()
pongthread ping pong n =
    if n <= 0
        then pure ()
        else do
            takeMVar pong
            putStrLn "pong"
            putMVar ping ()
            pongthread ping pong (n - 1)

main :: IO ()
main = do
    ping <- newEmptyMVar
    pong <- newEmptyMVar
    done <- newEmptyMVar
    forkIO (do
        pongthread ping pong 5
        putMVar done ())
    putMVar ping ()
    pingpong ping pong 5
    takeMVar done
    putStrLn "done"
