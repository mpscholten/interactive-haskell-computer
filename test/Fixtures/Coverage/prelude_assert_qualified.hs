-- Builtins-removal: assert must resolve via an explicit named import
-- from Control.Exception (which re-exports GHC.Base -> GHC.Internal.Base),
-- exercising the import-list path so the import itself is what brings it
-- into scope (independent of the bare-name fallback).  Assertions are
-- disabled in the interpreter, so @assert _ v == v@.
module Main where

import Control.Exception (assert)

main :: IO ()
main = do
    print (assert True (42 :: Int))
    let x = assert (1 + 1 == 2) "ok"
    putStrLn x
