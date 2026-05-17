-- @sqrt@ source-loaded from @GHC.Internal.Float@.
--
-- The bare-name @sqrt@ shim (@("sqrt", unaryOpFloat sqrt)@) was
-- dropped from @IHC.Builtins.builtins@ per CLAUDE.md's "Builtin
-- modules: minimum surface only" rule.  Resolution now flows through
-- the source-loaded @class Floating@ in @GHC.Internal.Float@:
--
--   * the ("sqrt","Floating") class-method seed in
--     'IHC.TypeGlobals.seedBuiltinClassMethodSigs' lets the
--     env-fallback's 'tryClassMethodFromRegistry' synthesise a
--     'classMethodDispatcher' for @sqrt@ on demand,
--   * 'triggerCoreInstanceLoad' source-loads the @Floating Double@
--     instance whose body is @sqrt x = sqrtDouble x@ (Float.hs:746),
--   * @sqrtDouble (D# x) = D# (sqrtDouble# x)@ (Float.hs:1578) bottoms
--     on the carved-out @sqrtDouble#@ GHC.Prim primop (no .hs source).
module Main where

main :: IO ()
main = do
    print (sqrt (2.0 :: Double))
    print (sqrt 16.0 :: Double)
    print (sqrt 0.0 :: Double)
