module Main where

main = pure (outer 8)

outer x = inner x
  where
    inner :: Integral i => i -> i
    inner y = y - 1
