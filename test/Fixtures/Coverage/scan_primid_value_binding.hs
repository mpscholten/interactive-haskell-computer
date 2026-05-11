{-# LANGUAGE MagicHash #-}
module Main where

-- Value-level MagicHash bindings: the name ends with '#' and is lexed as
-- TkPrimId, not TkIdent.  The binding scanner must recognise these as
-- top-level bindings so they can be discovered by env-fallback / by name.
-- This shape appears in GHC.Classes (compareInt#, compareWord#, divInt#,
-- modInt#, divModInt# etc.).

myInc# :: Int -> Int
myInc# x = x + 1

isPositive# :: Int -> Bool
isPositive# n = n > 0

main :: IO ()
main = do
  print (myInc# 41)
  print (isPositive# 5)
  print (isPositive# (-3))
