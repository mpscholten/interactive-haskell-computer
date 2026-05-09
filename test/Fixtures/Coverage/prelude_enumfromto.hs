-- Builtins-removal: @enumFromTo@ must resolve via the source-loaded
-- @class Enum@ in @GHC.Enum@ / @GHC.Internal.Enum@, not the historical
-- @enumFromToB@ shim.  Exercises the bare-name path that
-- @IHC.Parser@ desugars @[lo..hi]@ into via
-- @EApp (EApp (EVar "enumFromTo") lo) hi@ at @Parser.hs:3835@.
--
-- Resolution flows through 'lookupEnvFallback' into the implicit
-- Prelude fallback chain, then 'tryClassMethodFromRegistry'
-- synthesises a 'classMethodDispatcher' for @enumFromTo@ on demand.
-- The dispatcher routes by the first argument's tag:
--
--   * VInt  -> @Enum Int.enumFromTo  = eftInt  x y@
--             (uses primops @>#@, @==#@, @+#@, @isTrue#@)
--   * VChar -> @Enum Char.enumFromTo = eftChar (ord# x) (ord# y)@
--             (uses primops @ord#@, @chr#@)
--
-- Both instance overrides are pure Haskell in
-- @~/.cache/ihc/sources/base-4.19.0.0/GHC/Enum.hs@ and depend only
-- on primops already implemented in @IHC.Builtins@.
module Main where

main :: IO ()
main = do
    -- Int range, ascending
    print [1 .. 5 :: Int]
    -- Empty Int range (lo > hi)
    print [10 .. 3 :: Int]
    -- Singleton range
    print [7 .. 7 :: Int]
    -- Char range as [Char] (printed as String)
    putStrLn ['a' .. 'e']
    -- Char range with print (uses Show instance)
    print ['A' .. 'D']
    -- enumFromTo as a function value, not via syntax
    print (enumFromTo (3 :: Int) 6)
    -- Range used with map
    print (map (* 2) [1 .. 4 :: Int])
