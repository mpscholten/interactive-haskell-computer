-- Builtins-removal: concatMap must resolve via the source-loaded
-- Prelude re-export (Foldable.concatMap / GHC.Internal.List.concatMap),
-- not the historical concatMapB shim.
-- Exercises the bare-name path: not explicitly imported, so
-- resolution falls through 'lookupEnvFallback' into the Prelude
-- fallback chain.
module Main where

main :: IO ()
main = do
    print (concatMap (\x -> [x, x * 10]) [1, 2, 3])
    print (concatMap (\c -> [c, c]) "ab")
    print (concatMap (\xs -> xs) [[1, 2], [], [3]])
