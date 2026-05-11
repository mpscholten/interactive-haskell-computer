{-# LANGUAGE BangPatterns #-}
-- Locks down the bang-let multi-binding scope fix in
-- @IHC.Parser.parseOneLetItem@.
--
-- Before the fix, @let !d = ...; go ... = ... d ...@ desugared the
-- bang binding via a @case@-projection that only bound @d@ inside
-- the alt's body — not in the surrounding @ELet@ siblings — so the
-- inner @go@ definition saw @d@ as unbound.  IHC ignores the
-- strictness annotation anyway (Parser.hs:20), so the fix strips
-- @PBang (PVar n)@ to a normal @Left (n, e)@ binding, restoring
-- correct lexical scoping for the let group.
--
-- This shape appears throughout the source-loaded
-- @GHC.Enum.efdtIntUp@ / @efdtIntDn@ / @efdtCharUp@ / @efdtCharDn@
-- helpers used by stepped-range syntax @[lo, step .. hi]@.
module Main where

f :: Int -> Int
f n =
    let !d = n + 1
        go x = if x > 5 then x else go (x + d)
    in go 0

-- Two bang bindings in the same group, plus a non-fn binding that
-- references both.  All three share scope.
g :: Int -> Int
g n =
    let !a = n * 2
        !b = a + 3
        c  = a + b
    in c

main :: IO ()
main = do
    print (f 1)   -- 6
    print (g 5)   -- 23 (a=10, b=13, c=23)
