-- NegativeLiterals (always-on per IHC's parser): @-N@
-- /immediately/ following a function head is parsed as a single
-- negative-literal argument, not as binary subtraction.  This
-- mirrors GHC's @{-# LANGUAGE NegativeLiterals #-}@ behaviour
-- and is required by ghc-bignum source like
-- @intToInt64# INT_MINBOUND#@ where the CPP macro expands to a
-- @-0x...@ value at the @intToInt64#@ application site.
--
-- Adjacency check: only when there's NO whitespace between the
-- '-' and the digit.  @f - 1@ (with space) stays as binary
-- subtraction; only @f -1@ \/ @f -1.5@ \/ @f -0xff@ becomes
-- @f (-1)@ \/ etc.
module Main where

addOne :: Int -> Int
addOne n = n + 1

main :: IO ()
main = do
    -- Standard unary-minus position
    print (negate 5 :: Int)
    -- User function applied to negative-literal argument
    print (addOne -5)
    -- negate followed by negative literal still works
    print (negate -10 :: Int)
    print (negate -42 :: Int)
    -- Hex negative literals (the actual use-case)
    print (addOne -0xff)
    -- Subtraction with explicit spaces still works
    let x = 10 :: Int
    print (x - 5)
