-- Builtins-removal canary: @class Bits@ bitwise core via bare names.
--
-- The bare-name shims for @.&. .|. xor complement shiftL shiftR@
-- were dropped from @IHC.Builtins.builtins@ per CLAUDE.md's
-- "Builtin modules: minimum surface only" rule.  Resolution now
-- flows through the source-loaded @instance Bits Int@ in
-- @ghc-internal/GHC/Internal/Bits.hs@ (Bits.hs:444):
--
--   * 'IHC.TypeGlobals.seedBuiltinClassMethodSigs' seeds the
--     method→class map (@.&.@…@shiftR@ ↦ @Bits@), so the
--     env-fallback's 'tryClassMethodFromRegistry' synthesises a
--     'classMethodDispatcher' on first reference without a
--     Prelude-scope walk.
--   * The dispatcher's 'triggerCoreInstanceLoad' source-loads the
--     @Bits Int@ instance, whose method bodies bottom on the
--     @andI# / orI# / xorI#@ primops (registered) and the
--     @notI#@ primop (GHC.Prim carve-out, registered alongside
--     them).  @shiftL@/@shiftR@ ride @iShiftL#@/@iShiftRA#@,
--     which source-load from Base.hs onto @uncheckedIShiftL#@ /
--     @uncheckedIShiftRA#@ (registered).
--
-- Lock down each method the removed shims used to handle.
module Main where

main :: IO ()
main = do
    print (5 .&. 3)          -- 0b101 & 0b011 = 0b001 = 1
    print (5 .|. 2)          -- 0b101 | 0b010 = 0b111 = 7
    print (xor 6 3)          -- 0b110 ^ 0b011 = 0b101 = 5
    print (complement 0)     -- ~0 = -1 (two's complement Int)
    print (shiftL 1 4)       -- 1 << 4 = 16
    print (shiftR 32 2)      -- 32 >> 2 = 8
