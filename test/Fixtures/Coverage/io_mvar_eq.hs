import Control.Concurrent.MVar

main :: IO ()
main = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    print (m1 == m1)
    print (m1 == m2)
