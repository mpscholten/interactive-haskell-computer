-- Phase C.1 (builtins-removal): runIdentity must resolve even when not
-- explicitly imported.  This exercises the preludeDirectOwner fallback
-- in Scheduler.resolveBarePrelude that force-loads Data.Functor.Identity
-- for a bare runIdentity reference.
import Data.Functor.Identity (Identity(..))

main :: IO ()
main = print (runIdentity (Identity 7))
