{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC.TypeLits

-- Cons-pattern match on a promoted Symbol list; returns the head.
type family HeadSym (xs :: [Symbol]) :: Symbol where
  HeadSym (x ': rest) = x

main :: IO ()
main = putStrLn (symbolVal @(HeadSym '["hello", "world"]) undefined)
