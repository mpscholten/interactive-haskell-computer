import Data.Array.ST (runSTArray, newArray)
import Data.Array (bounds)

main :: IO ()
main = do
    let arr = runSTArray (newArray (0, 3) (Nothing :: Maybe Int))
    print (bounds arr)
