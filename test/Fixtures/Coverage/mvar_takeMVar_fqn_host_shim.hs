-- Regression: 'Control.Concurrent.MVar.takeMVar' must dispatch to
-- the host primitive 'takeMVarB', not to the source-loaded body in
-- 'GHC.Internal.MVar' / 'GHC.MVar' that pattern-matches on the
-- 'MVar' constructor.
--
-- The runtime carries MVars as 'VPrimObj (PrimMVar _)', NOT as
-- 'VCon "MVar" [_]', so the source body @takeMVar (MVar mvar#) =
-- IO $ \s -> takeMVar# mvar# s@ pattern-fails the moment it's
-- entered.  When source-loaded, the pattern-match-failure exception
-- bubbles up through any enclosing try/catch (e.g. the worker thread
-- in 'Control.AutoUpdate.Thread.mkAutoUpdateHelper'), the worker
-- silently dies, and the main thread blocks forever on its
-- response 'MVar' — observed end-to-end as warp's date cache never
-- producing a date and the accept loop never reaching curl.
--
-- The unqualified 'takeMVar' was already host-shimmed; this fixture
-- locks in that the *qualified* import path
-- ('Control.Concurrent.MVar.takeMVar', 'GHC.Internal.MVar.takeMVar',
-- etc.) also resolves to the host primitive when source code reaches
-- for it via an explicit import or a re-export chain.
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
