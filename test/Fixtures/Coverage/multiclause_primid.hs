-- Top-level multi-clause function with a MagicHash (TkPrimId) name.
-- IHC.Scan.collectMoreClauses must accept TkPrimId at column 1 so the
-- second/third clauses are grouped into the same BindingLhs.  Without
-- this, each clause would overwrite the previous in the binding Map
-- (Scan.hs:831 Map.insert) and the function would only retain its
-- LAST clause — the rest would silently disappear.
--
-- Real-world impact: ghc-bignum's @integerToInt#@ in
-- @GHC.Num.Integer@ is defined this way:
--
--   integerToInt# (IS i) = i
--   integerToInt# (IP b) = word2Int# (bigNatToWord# b)
--   integerToInt# (IN b) = negateInt# ...
--
-- With the bug, calling @integerToInt# (IS 5)@ failed with
-- @Non-exhaustive patterns in function: [[PCon "IN" [PVar "b"]]]@
-- because only the IN clause survived parsing.  This in turn broke
-- the source-loaded @Num Int.fromInteger i = I# (integerToInt# i)@
-- chain.
{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Types (Int(..))

-- Multi-clause TkPrimId binding.  The name ends in @#@ so the lexer
-- emits 'TkPrimId', not 'TkIdent'.  All three clauses must group into
-- one BindingLhs.
foo# :: Int -> Int
foo# 0 = 100
foo# 1 = 200
foo# n = n + 1000

main :: IO ()
main = do
    print (foo# 0)
    print (foo# 1)
    print (foo# 7)
    print (foo# 42)
