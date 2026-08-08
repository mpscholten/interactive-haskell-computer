-- Phase 5 north-star smoke: the original end-to-end target of the
-- full source-loaded ghc-bignum Integer roadmap.
--
-- From plans/full-ghc-bignum-source-load.md (final paragraph):
--
--   "End-to-end smoke: 'print (floor (1.5 :: Double) :: Int)' prints
--    '1' after the float→Int graduation, going through source-loaded
--    Integer arithmetic the whole way."
--
-- Today this works through the source-loaded RealFrac / RealFloat Double
-- path, bottoming out on the Double#/Integer primops in 'IHC.Builtins'
-- rather than ordinary Haskell API shims.
--
-- This fixture pins the visible behaviour: any regression in the
-- float→Int path (positive, negative, exact, boundary) is caught
-- through the source-loaded float-to-Int path.
--
-- See plans/full-ghc-bignum-source-load.md (Phase 5).
module Main where

main :: IO ()
main = do
    -- Positive halves and odd-rounding
    print (floor    ( 1.5 :: Double) :: Int)   -- 1   ← the canonical smoke
    print (ceiling  ( 1.5 :: Double) :: Int)   -- 2
    print (round    ( 2.7 :: Double) :: Int)   -- 3
    print (truncate ( 2.9 :: Double) :: Int)   -- 2
    -- Negative halves
    print (floor    (-1.5 :: Double) :: Int)   -- -2
    print (ceiling  (-1.5 :: Double) :: Int)   -- -1
    print (truncate (-2.9 :: Double) :: Int)   -- -2
    print (round    (-2.7 :: Double) :: Int)   -- -3
    -- Exact integral Doubles
    print (floor    ( 5.0 :: Double) :: Int)   -- 5
    print (ceiling  ( 5.0 :: Double) :: Int)   -- 5
    print (round    ( 5.0 :: Double) :: Int)   -- 5
    print (truncate ( 5.0 :: Double) :: Int)   -- 5
    -- Banker's rounding (round-half-to-even): both should produce 2
    print (round    ( 2.5 :: Double) :: Int)   -- 2 (even)
    print (round    ( 3.5 :: Double) :: Int)   -- 4 (even)
    -- Zero
    print (floor    ( 0.0 :: Double) :: Int)   -- 0
    print (ceiling  ( 0.0 :: Double) :: Int)   -- 0
