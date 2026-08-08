import Data.IORef

main :: IO ()
main = do
    r1 <- newIORef (1 :: Int)
    r2 <- newIORef (1 :: Int)
    print (r1 == r1)
    print (r1 == r2)
