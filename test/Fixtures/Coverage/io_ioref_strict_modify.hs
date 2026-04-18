-- modifyIORef' with mixed transformations: succ, (*), lambda;
-- plus a list-building pattern. Complements baselib_data_ioref_cycle
-- which uses top-level sections only.
import Data.IORef

main :: IO ()
main = do
    ref <- newIORef (0 :: Int)
    modifyIORef' ref succ
    modifyIORef' ref succ
    modifyIORef' ref (\x -> x * x)
    readIORef ref >>= print
    lref <- newIORef ([] :: [Int])
    mapM_ (\n -> modifyIORef' lref (n :)) [1, 2, 3]
    readIORef lref >>= print
