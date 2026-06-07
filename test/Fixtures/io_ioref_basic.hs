import Data.IORef

main = do
    r <- newIORef 99
    v <- readIORef r
    print v
-- expects:
--   99
