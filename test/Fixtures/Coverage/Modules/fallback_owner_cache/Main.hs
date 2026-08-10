module Main where

import OwnerA (fromA)
import OwnerB (fromB)

main :: IO Int
main = pure (fromA + fromA + fromB + fromB)
