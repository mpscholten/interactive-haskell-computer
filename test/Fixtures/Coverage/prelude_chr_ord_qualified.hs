-- Builtins-removal companion to prelude_chr_ord: explicit-import path.
-- Source-loading must succeed when @chr@ / @ord@ are brought in via
-- 'import Data.Char (chr, ord)' (the user-facing re-export of
-- @GHC.Internal.Base.ord@ / @GHC.Internal.Char.chr@).
module Main where

import Data.Char (chr, ord)

main :: IO ()
main = do
    print (ord 'Z')
    print (chr 65)
    print (chr (ord 'Z' + 1))
