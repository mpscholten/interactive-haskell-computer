-- Builtins-removal companion to prelude_length: explicit-import
-- path.  Source-loading must succeed when length is brought in
-- via 'import GHC.List' (the user-facing re-export of
-- GHC.Internal.List).
module Main where

import GHC.List (length)

main :: IO ()
main = do
    print (length [1, 2, 3])
    print (length [True, False, True, True])
