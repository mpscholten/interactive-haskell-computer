-- Phase C.1 (builtins-removal): Data.Functor.Identity.runIdentity must
-- resolve via source-loaded field accessor, not the historical shim.
-- Exercises the import-list path: both Identity(..) and runIdentity are
-- explicitly named in the import list.
import Data.Functor.Identity (Identity(..), runIdentity)

main :: IO ()
main = do
    print (runIdentity (Identity 42))
    print (runIdentity (Identity "hello"))
    print (runIdentity (Identity { runIdentity = 100 }))
