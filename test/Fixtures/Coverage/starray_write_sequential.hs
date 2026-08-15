-- Sequential STArray writeArray in a do (evalDo SExpr), no mapM_ / >>.
-- Contrast st_mapm_writearray.hs (Warp indexResponseHeader).
import Control.Monad.ST
import Data.Array.ST
import Data.Array (Array, (!))

main :: IO ()
main = do
    let arr :: Array Int (Maybe Int)
        arr = runSTArray $ do
            a <- newArray (0, 3) Nothing
            writeArray a 0 (Just 1)
            writeArray a 2 (Just 7)
            return a
    print (arr ! 0)
    print (arr ! 2)
