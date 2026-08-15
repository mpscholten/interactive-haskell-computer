-- runSTArray + mapM_ writeArray (Warp indexResponseHeader).
-- sendResponse of responseLBS after accept needs
--   indexResponseHeader = runSTArray $ newArray >> mapM_ insert
-- Sequential writeArray in a do (evalDo SExpr) is GREEN.
-- mapM_ = foldr ((>>) . f) (return ()) sequences writeArray via
-- source Monad.>>.  writeArray is a State#-shaped VFun (tag
-- <function>), so >> must stay ST (value-directed / lastMonadicCarrier
-- / wrap of ST dos), not default to ParsecT.
import Control.Monad.ST
import Data.Array.ST
import Data.Array (Array, (!))

main :: IO ()
main = do
    let arr :: Array Int (Maybe Int)
        arr = runSTArray $ do
            a <- newArray (0, 3) Nothing
            mapM_ (\(i, v) -> writeArray a i (Just v)) [(2, 7), (0, 1)]
            return a
    print (arr ! 0)
    print (arr ! 2)
