-- Same leftover as Warp composeHeader without HTTP/Warp names:
-- imported list synonym in a signature + foldl' + [] argument.
import Data.List (foldl')
import Syn (Xs)

go :: Xs -> Int
go xs = 17 + 2 + foldl' (\n _ -> n) 0 xs

main :: IO Int
main = return (go [])
