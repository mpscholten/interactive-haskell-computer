module Main where

data Box = Box Int

(Box x) `eqBox` (Box y) = x == y
(Box x) `addBox` (Box y) = Box (x + y)

main :: IO ()
main = do
  print (Box 3 `eqBox` Box 3)
  print (Box 3 `eqBox` Box 4)
  case Box 5 `addBox` Box 7 of Box n -> print n
