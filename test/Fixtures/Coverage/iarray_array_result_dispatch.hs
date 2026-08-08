import Data.Array.IArray (Array, array, (!))

main :: IO ()
main = do
    let arr = array (0, 2) [(1, "ok")] :: Array Int String
    print (arr ! 1)
