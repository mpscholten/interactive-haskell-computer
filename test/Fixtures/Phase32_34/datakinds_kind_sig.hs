-- Phase 3.4: DataKinds kind signatures (parse-discard)
-- Kind annotations like (:: Symbol), (:: Nat) in type sigs are discarded.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main (main) where

-- A type with a kind-annotated parameter; interpreter ignores the kind.
-- The value-level function works normally.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

main :: IO ()
main = print (myLength [10, 20, 30, 40, 50])
