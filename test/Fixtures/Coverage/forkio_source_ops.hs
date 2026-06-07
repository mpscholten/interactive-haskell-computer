import Control.Concurrent.MVar
import qualified Control.Concurrent as C
import qualified GHC.Conc.Sync as GHC
import qualified GHC.Internal.Conc.Sync as Internal

main :: IO ()
main = do
    m1 <- newEmptyMVar
    _ <- C.forkIO $ putMVar m1 "control"
    takeMVar m1 >>= putStrLn

    m2 <- newEmptyMVar
    _ <- GHC.forkIO $ putMVar m2 "ghc"
    takeMVar m2 >>= putStrLn

    m3 <- newEmptyMVar
    _ <- Internal.forkIOWithUnmask $ \unmask ->
        unmask (putMVar m3 "with-unmask")
    takeMVar m3 >>= putStrLn
