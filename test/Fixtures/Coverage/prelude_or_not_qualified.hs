-- Builtins-removal: explicit-import path for (||) / 'not'.
-- Source-loading must succeed when these are brought in via
-- 'import GHC.Classes' (the user-facing source module).
module Main where

import GHC.Classes ((||), not)

main :: IO ()
main = do
    print (not False || True)
    print (not (False || True))
    print (not True || not True)
