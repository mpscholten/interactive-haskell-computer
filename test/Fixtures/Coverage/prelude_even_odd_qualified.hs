-- Builtins-removal: even/odd must resolve via an explicit named import
-- from GHC.Real, exercising the import-list path so the import itself
-- is what brings them into scope (independent of the bare-name fallback
-- in 'preludeDirectOwner').
module Main where

import GHC.Real (even, odd)

main :: IO ()
main = mapM_ (\n -> print (even n, odd n)) [0,1,2,3,4]
