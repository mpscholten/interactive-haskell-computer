import Data.Array (Array, array, (!))

main :: IO ()
main = do
    let arr = array (0, 2) [(i, "ok") | i <- [0 .. 2]] :: Array Int String
    print (arr ! 1)
