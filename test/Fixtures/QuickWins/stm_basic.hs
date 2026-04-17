-- Basic STM: newTVarIO / writeTVar / readTVarIO bridged to IO.
-- See commit 1ed2881 (ST ≈ IO) — same strategy: ihc's single-threaded
-- eval makes STM a straight IO bridge.
import Control.Concurrent.STM
main :: IO ()
main = do
    v <- newTVarIO (0 :: Int)
    atomically (writeTVar v 42)
    x <- readTVarIO v
    print x
