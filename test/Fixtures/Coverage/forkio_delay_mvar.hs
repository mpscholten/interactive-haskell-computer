-- Regression: forkIO must return to the parent; a child threadDelay
-- followed by putMVar must wake the parent's takeMVar.
--
-- A yield-spin on a foreign black-hole holds the capability, so the
-- child never finishes delay# / putMVar and the parent never resumes.
-- Warp accept hangs the same way: the forked client never connect()s.
import Control.Concurrent

main = do
  putStrLn "start"
  m <- newEmptyMVar
  _ <- forkIO $ do
    putStrLn "child"
    threadDelay 100000
    putMVar m ()
  takeMVar m
  putStrLn "parent"
