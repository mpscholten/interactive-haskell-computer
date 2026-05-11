module Main where
import GHC.Classes (eqInt, geInt, compareInt)
main :: IO ()
main = do
  print (eqInt 3 3)
  print (geInt 3 2)
  print (compareInt 3 2)
