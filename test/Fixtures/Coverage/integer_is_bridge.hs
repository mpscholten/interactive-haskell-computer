-- The IS / IP / IN constructor bridge in 'IHC.Eval.matchPat'.
--
-- @ghc-bignum@'s 'Integer' is @data Integer = IS !Int# | IP
-- !BigNat# | IN !BigNat#@.  IHC's runtime stores Integer values
-- as 'VInt' (Int64-range) or 'VInteger' (arbitrary precision).
-- Source-loaded code that pattern-matches @case n of IS k -> ...@
-- needs to see the underlying primitive — same transparent-
-- constructor trick already used for @I#@ / @F#@ / @D#@ etc.
--
-- This fixture pins the IS arm of the bridge end-to-end:
-- import the 'Integer' data constructors from ghc-bignum, build
-- a value via the IS constructor, pattern-match it back out.
-- Without the matchPat bridge, the pattern @IS k@ would fail
-- against an underlying 'VInt' (the bridge unwraps it,
-- exposing the underlying Int# the source code expects).
module Main where

import GHC.Num.Integer (Integer(..))
import GHC.Exts (Int(..))

unwrap :: Integer -> Int
unwrap (IS k) = I# k
unwrap _      = error "unwrap: not IS"

main :: IO ()
main = do
    -- Construct via IS, pattern back out
    print (unwrap (IS 42#))
    print (unwrap (IS 0#))
    print (unwrap (IS -1#))
    -- Construct via the integer-literal path (parser produces
    -- VInt internally), pattern as IS — bridge fires
    let n :: Integer
        n = 7
    print (unwrap n)
