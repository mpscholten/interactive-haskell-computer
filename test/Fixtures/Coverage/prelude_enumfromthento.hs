-- Builtins-removal: @enumFromThenTo@ must resolve via the source-loaded
-- @class Enum@.  @IHC.Parser@ desugars @[lo, step .. hi]@ into
-- @EApp (EApp (EApp (EVar "enumFromThenTo") lo) step) hi@ at
-- @Parser.hs:3853@.
--
-- Exercises the 3-arg dispatcher path independently from the
-- single-bound @enumFromTo@ — a bug in one would not necessarily
-- surface in the other.
--
-- @Enum Int.enumFromThenTo (I# x1) (I# x2) (I# y) = efdtInt x1 x2 y@
-- in @~/.cache/ihc/sources/base-4.19.0.0/GHC/Enum.hs@.  @efdtInt@
-- delegates to @efdtIntUp@/@efdtIntDn@, both of which use
-- @let !delta = x2 -# x1@ + an inner @go_up@/@go_dn@ helper that
-- references @delta@ — that pattern only resolves correctly with the
-- companion bang-let scope fix in @Parser.parseOneLetItem@.
module Main where

main :: IO ()
main = do
    -- Ascending step
    print [1, 3 .. 10 :: Int]
    -- Descending step
    print [10, 8 .. 0 :: Int]
    -- Step that overshoots immediately (yields just lo)
    print [1, 5 .. 3 :: Int]
    -- Negative step but lo < hi → empty
    print [1, -1 .. 5 :: Int]
    -- enumFromThenTo as a function value
    print (enumFromThenTo (0 :: Int) 5 25)
