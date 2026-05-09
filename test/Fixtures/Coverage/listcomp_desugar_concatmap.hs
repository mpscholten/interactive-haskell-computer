-- Builtins-removal: load-bearing fixture for the concatMap shim
-- removal.  Verifies the list-comprehension desugar path:
-- IHC.Parser.qualWrap (Parser.hs:3915) desugars
-- @[y | x <- xs, y <- ys]@ to @concatMap (\\x -> ...) xs@,
-- so removing the host-shim must still allow source-loaded
-- GHC.Internal.List.concatMap to be reached via the bare-name
-- env fallback.
--
-- A second path — @Monad []@ do-notation desugaring through
-- GHC.Internal.Base:1748's @xs >>= f = [y | x <- xs, y <- f x]@
-- — currently fails with @concatMap: not a list: <IO>@ even
-- with the shim, so it is not exercised here.  See the inline
-- comment at GHC.Internal.Base:1748 and the spawned follow-up
-- task once the @Monad []@ dispatch is fixed.
module Main where

main :: IO ()
main = do
    -- Single-generator list comp (translates to concatMap)
    print [x * x | x <- [1, 2, 3, 4]]
    -- Multi-generator list comp (nested concatMap)
    print [y | x <- [1, 2, 3], y <- [x, x * 10]]
    -- List comp with guard (translates to concatMap + if)
    print [x | x <- [1 .. 10 :: Int], x `mod` 2 == 0]
