-- Builtins-removal: gcd must resolve via the source-loaded
-- Prelude re-export (GHC.Internal.Real), not the historical
-- @binOpInt gcd@ shim.  The recursive body uses 'abs' (graduated
-- in Phase E) and 'rem' (still shimmed) plus first-order pattern
-- matching on 0.
module Main where

main :: IO ()
main = do
    print (gcd 12 8     :: Int)
    print (gcd 100 75   :: Int)
    print (gcd 7 13     :: Int)
    print (gcd 0 5      :: Int)
    print (gcd 5 0      :: Int)
    print (gcd 144 60   :: Int)
