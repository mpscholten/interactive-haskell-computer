-- Builtins-removal: (||) and 'not' must resolve via the
-- source-loaded Prelude re-export (GHC.Classes), not the
-- historical orB / notB shims.  Companion to the (&&)
-- graduation in PR #126 (640ff78).
module Main where

main :: IO ()
main = do
    -- not
    print (not True)
    print (not False)
    print (not (not True))
    -- (||)
    print (True  || False)
    print (False || True)
    print (False || False)
    print (True  || True)
    -- mixed
    print (not (True  || False))
    print (not (False || False))
