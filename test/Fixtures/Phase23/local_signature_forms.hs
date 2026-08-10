{-# LANGUAGE ExplicitForAll #-}
module Main where

main = pure (forms 3)

forms x =
    let { a, b :: Int; a = 1; b = 2 }
    in a + b + shadow x + multi x + bracedWhere x
  where
    shadow :: Int -> Int
    shadow n = let { shadow :: Int; shadow = n + 1 } in shadow

    multi :: forall a. Num a => a -> a
    multi 0 = 10
    multi n = n - 1

    bracedWhere n = f n where { f :: Int -> Int; f value = value }
