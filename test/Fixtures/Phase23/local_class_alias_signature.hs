module Main where

main = pure (useAdd 4)

useAdd x = add x 3
  where
    add :: Num a => a -> a -> a
    add = (+)
