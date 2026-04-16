import Control.Monad.ST
import Data.STRef
main :: IO ()
main = do
    let result = runST $ do
          ref <- newSTRef (0 :: Int)
          writeSTRef ref 42
          readSTRef ref
    print result
