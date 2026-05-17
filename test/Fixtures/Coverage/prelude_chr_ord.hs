-- @ord@ / @chr@ source-loaded (bare-name path).
--
-- The bare-name @ord@ / @chr@ shims were dropped from
-- @IHC.Builtins.builtins@ per CLAUDE.md's "Builtin modules: minimum
-- surface only" rule. Only the GHC.Prim primops @ord#@ / @chr#@ stay
-- host-backed. Resolution now flows through the source bodies:
--
--   * @ord :: Char -> Int; ord (C# c#) = I# (ord# c#)@ in
--     @ghc-internal/GHC/Internal/Base.hs@,
--   * @chr :: Int -> Char; chr i\@(I# i#) | isTrue# (...) = C# (chr# i#)
--     | otherwise = errorWithoutStackTrace ...@ in
--     @ghc-internal/GHC/Internal/Char.hs@.
--
-- Both pattern-unwrap the @C#@ / @I#@ constructors and bottom out on
-- the kept @ord#@ / @chr#@ primops via the env-fallback path
-- (EVar -> lookupEnvFallback -> tryAnyModuleBareSlot).
main :: IO ()
main = do
    print (ord 'A')
    print (chr 97)
    print (map ord "Hi")
    print (chr (ord 'a' + 1))
