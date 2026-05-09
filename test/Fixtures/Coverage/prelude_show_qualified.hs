-- Builtins-removal companion to prelude_show: explicit-import path.
-- Source-loading must succeed when @show@ is brought in via
-- 'import GHC.Show' (the user-facing re-export of
-- @GHC.Internal.Show@).
module Main where

import GHC.Show (show)

main :: IO ()
main = do
    putStrLn (show (123 :: Int))
    putStrLn (show 'q')
    putStrLn (show True)
