import Data.Array.ST (runSTArray, newArray, writeArray)
import Data.Array (bounds, (!))

main :: IO ()
main = do
    let arr = runSTArray $ do
                a <- newArray (0, 2) (0 :: Int)
                writeArray a 1 42
                pure a
    print (bounds arr)
    print (arr ! 1)
