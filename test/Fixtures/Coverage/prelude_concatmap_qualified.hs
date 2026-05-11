-- Builtins-removal companion to prelude_concatmap: explicit-import
-- path.  Source-loading must succeed when concatMap is brought in
-- via 'import GHC.List' (the user-facing re-export of
-- GHC.Internal.List).
module Main where

import GHC.List (concatMap)

main :: IO ()
main = do
    print (concatMap (\x -> [x + 1]) [10, 20, 30])
