-- Unboxed-sum runtime: (# (# #) | a #) construction + pattern
-- matching, exercised through ghc-bignum's two unboxed-sum-returning
-- BigNat# primops that were deferred from Phase 2:
--
--   bigNatSub         :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)
--   bigNatIsPowerOf2# :: BigNat# -> (# (# #) | Word# #)
--
-- IHC encodes every unboxed sum as @VCon "(#|#)" [VInt tag, payload]@
-- (tag = 1-based alternative index).  The parser desugars both the
-- construction @(# x | #)@ / @(# | x #)@ and the patterns
-- @(# (# #) | #)@ / @(# | x #)@; matchPat's generic VCon zip checks
-- the tag (a PLit Int sub-pattern) and binds the payload.
--
-- This fixture also pins the empty/left injection carrying the
-- nullary unboxed tuple @(# #)@ — the "Nothing"/underflow arm.
module Main where

import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatSub
    , bigNatIsPowerOf2#
    , bigNatToWord#
    )
import GHC.Exts (Word(..))

-- bigNatSub: right injection carries the difference; left injection
-- (empty, carrying (# #)) signals would-underflow.
subShow :: Word -> Word -> String
subShow (W# a) (W# b) =
  case bigNatSub (bigNatFromWord# a) (bigNatFromWord# b) of
    (# (# #) | #) -> "underflow"
    (# | bn #)    -> "ok " ++ show (W# (bigNatToWord# bn))

-- bigNatIsPowerOf2#: right injection carries the exponent k (2^k);
-- left injection means "not a power of two" (incl. zero).
pow2Show :: Word -> String
pow2Show (W# n) =
  case bigNatIsPowerOf2# (bigNatFromWord# n) of
    (# (# #) | #) -> "no"
    (# | k #)     -> "2^" ++ show (W# k)

main :: IO ()
main = do
    -- bigNatSub — right injection (success)
    putStrLn (subShow 10 3)      -- ok 7
    putStrLn (subShow 5 5)       -- ok 0   (a == b boundary)
    putStrLn (subShow 100 1)     -- ok 99
    -- bigNatSub — left injection (would-underflow)
    putStrLn (subShow 3 10)      -- underflow
    putStrLn (subShow 0 1)       -- underflow
    -- bigNatIsPowerOf2# — right injection (is 2^k)
    putStrLn (pow2Show 1)        -- 2^0
    putStrLn (pow2Show 2)        -- 2^1
    putStrLn (pow2Show 8)        -- 2^3
    putStrLn (pow2Show 1024)     -- 2^10
    -- bigNatIsPowerOf2# — left injection (not a power of two)
    putStrLn (pow2Show 0)        -- no
    putStrLn (pow2Show 7)        -- no
    putStrLn (pow2Show 1000)     -- no
