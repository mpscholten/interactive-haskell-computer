-- Regression: 'Control.Concurrent.MVar.takeMVar' must source-load
-- through 'GHC.Internal.MVar' / 'GHC.MVar' and bottom out on the
-- 'takeMVar#' primop.
--
-- The low-level runtime may still carry MVars as raw
-- 'VPrimObj (PrimMVar _)' values, so the pattern bridge must let the
-- source clause @takeMVar (MVar mvar#) = IO $ \s -> takeMVar# mvar# s@
-- see the underlying primitive object.
import qualified Control.Concurrent.MVar as M
import Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do
    mv <- M.newEmptyMVar
    done <- M.newEmptyMVar
    _ <- forkIO $ do
        v <- M.takeMVar mv          -- qualified import path
        M.putMVar done (v + 1)
    M.putMVar mv (41 :: Int)
    r <- M.takeMVar done
    print r
