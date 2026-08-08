import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

main = print (runST (newSTRef (0 :: Int) >>= \r -> writeSTRef r 5 >> readSTRef r))
