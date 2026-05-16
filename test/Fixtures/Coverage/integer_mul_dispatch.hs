-- integerMul source-load + big-Integer class dispatch.
--
-- Two coupled fixes exercised here:
--
--  1. Parser: a NegativeLiterals sub-pattern as a constructor
--     argument, e.g. @integerMul x (IS -1#) = integerNegate x@ at
--     ghc-bignum GHC/Num/Integer.hs:431.  IHC's 'collectArgs'
--     previously bailed on @TkMinus@ (treating it as the infix @-@
--     separator), so @integerMul@ failed to parse entirely and any
--     @import GHC.Num.Integer (integerMul)@ — or @Num Integer.(*)@
--     class dispatch routing through it — hit
--     "unresolved target GHC.Num.Integer.integerMul".
--     'integerAdd' has no such pattern, which is why it always
--     worked and 'integerMul' didn't.
--
--  2. Scheduler: a source-loaded sibling (integerMul's @IP@/@IN@
--     arms) calls @bigNatMulWord#@ / @bigNatMul@, whose FV is
--     import-rewritten to the FQN @GHC.Num.BigNat.bigNatMulWord#@.
--     That FQN isn't a bare-name baseEnv key, so it used to
--     source-load the ByteArray#-based body and crash with
--     "sizeofByteArray#: not a ByteArray: <BigNat# …>".
--     resolveFallbackSource now serves the host shim for
--     @GHC.Num.{BigNat,WordArray,…}@ FQNs (the Phase 2 BigNat#
--     carve-out, qualified-resolution path).
--
-- Net effect: Num Integer / Integral Integer arithmetic via class
-- dispatch now works for out-of-Int64 (VInteger) operands.
module Main where

main :: IO ()
main = do
    -- Small (in-Int64) Integer arithmetic — Num Integer dispatch.
    print ((5 :: Integer) + 3)        -- 8
    print ((5 :: Integer) * 3)        -- 15
    print ((5 :: Integer) - 3)        -- 2
    print (negate (5 :: Integer))     -- -5
    -- Out-of-Int64 (VInteger) operands route through source-loaded
    -- integerMul / integerAdd → host bigNat* shims.
    let big = 18446744073709551616 :: Integer   -- 2^64
    print (big * 2)                   -- 2^65
    print (big + 1)                   -- 2^64 + 1
    print (big - 1)                   -- 2^64 - 1
    let a = 18446744073709551616 :: Integer     -- 2^64
        b = 4294967296           :: Integer     -- 2^32
    print (a * a)                     -- 2^128
    print (a * b)                     -- 2^96
    print (a `div` b)                 -- 2^32
    print (gcd a b)                   -- 2^32
    -- integerNegate via the (IS -1#) clause that previously
    -- failed to parse: multiplying by -1 negates.
    print (big * (-1))                -- -(2^64)
