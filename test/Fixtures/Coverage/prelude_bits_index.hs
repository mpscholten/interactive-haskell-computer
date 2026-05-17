-- @popCount@ / @bit@ / @testBit@ / @clearBit@ / @setBit@ source-loaded
-- from @GHC.Internal.Bits@.
--
-- The bare-name @class Bits@ index-op shims were dropped from
-- @IHC.Builtins.builtins@ per CLAUDE.md's "Builtin modules: minimum
-- surface only" rule.  Resolution now flows through the source-loaded
-- @class Bits@ in @ghc-internal/GHC/Internal/Bits.hs@:
--
--   * 'IHC.TypeGlobals.seedBuiltinClassMethodSigs' pre-seeds the
--     method→class mapping (@popCount@/@bit@/@testBit@/@clearBit@/
--     @setBit@ ↦ @Bits@) into 'globalMethodClassRef', so the
--     env-fallback's 'tryClassMethodFromRegistry' synthesises a
--     'classMethodDispatcher' on the first bare reference.
--   * The dispatcher routes to the @Bits Int@ instance / class
--     defaults, which express these via shifts, @.&.@, @.|.@,
--     @complement@, and the @popCnt#@ primop (still registered in
--     'IHC.Builtins').
--   * The Phase-F guard in 'IHC.Scheduler.buildOwnerLocalEnv' keeps
--     the source instance bodies' inner class-method calls hitting
--     the Int primops.
--
-- Bare-name path: no import; the names resolve via @preludeScope@ /
-- env-fallback.
main :: IO ()
main = do
    print (popCount (255 :: Int))
    print (bit 4 :: Int)
    print (testBit (5 :: Int) 0)
    print (testBit (5 :: Int) 1)
    print (clearBit (7 :: Int) 1)
    print (setBit (0 :: Int) 3)
