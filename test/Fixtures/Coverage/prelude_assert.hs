-- Builtins-removal: assert must resolve via the source-loaded Prelude
-- re-export (GHC.Internal.Base, where @assert _pred r = r@), not the
-- historical shim.  Exercises the bare-name path: assert is not
-- explicitly imported, so resolution falls through 'lookupEnvFallback'
-- into the Prelude fallback chain.  Assertions are disabled in the
-- interpreter, so @assert _ v == v@ regardless of the predicate.
module Main where

main :: IO ()
main = do
    print (assert True (42 :: Int))
    let x = assert (1 + 1 == 2) "ok"
    putStrLn x
