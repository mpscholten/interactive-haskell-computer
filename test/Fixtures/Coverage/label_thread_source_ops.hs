import Control.Concurrent
import Control.Concurrent.MVar
import qualified GHC.Conc.Sync as GHC
import qualified GHC.Internal.Conc.Sync as Internal

main :: IO ()
main = do
    tid <- myThreadId
    GHC.labelThread tid "main-thread"
    putStrLn "label-ghc"

    m <- newEmptyMVar
    _ <- forkIO $ do
        child <- myThreadId
        Internal.labelThread child "child-thread"
        putMVar m "label-internal"
    takeMVar m >>= putStrLn
