-- Builtins-removal: length must resolve via the source-loaded
-- Prelude re-export (Foldable.length / GHC.Internal.List.length),
-- not the historical lengthB shim.
-- Exercises the bare-name path: not explicitly imported, so
-- resolution falls through 'lookupEnvFallback' into the Prelude
-- fallback chain.
module Main where

main :: IO ()
main = do
    print (length [1, 2, 3, 4, 5])
    print (length ([] :: [Int]))
    print (length "hello")
    print (length [length [10, 20], length [30, 40, 50]])
