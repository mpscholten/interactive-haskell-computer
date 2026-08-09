{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

data T = T Int

pattern R { value } = T value

pattern RV { viewed } <- T (id -> viewed)
  where
    RV viewed = T viewed

main :: IO ()
main = case T 42 of
    R { value } -> case RV value of
        RV { viewed } -> print viewed
