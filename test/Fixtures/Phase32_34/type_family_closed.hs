-- Phase 3.2: closed type family (parse-discard)
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

-- Closed type family with a where block: all equations discarded.
type family Foo x where
  Foo Int  = Bool
  Foo Bool = Int
  Foo _    = ()

-- Value-level code that ignores the family.
answer :: Int
answer = 42

main :: IO ()
main = print answer
