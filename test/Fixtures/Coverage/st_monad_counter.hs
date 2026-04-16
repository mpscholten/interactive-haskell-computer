import Control.Monad.ST
import Data.STRef
sumTo :: Int -> Int
sumTo n = runST $ do
    ref <- newSTRef 0
    let loop i = if i > n
            then readSTRef ref
            else modifySTRef ref (+ i) >> loop (i + 1)
    loop 1
main :: IO ()
main = print (sumTo 100)
