-- Builtins-removal: even/odd must resolve via the source-loaded
-- Prelude re-export (GHC.Internal.Real), not the historical shim.
-- Exercises the bare-name path: neither function is explicitly
-- imported, so resolution falls through 'lookupEnvFallback' into
-- the Prelude fallback chain.
module Main where

main :: IO ()
main = mapM_ (\n -> print (even n, odd n)) [0,1,2,3,4]
