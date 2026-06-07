-- Regression canary for source-loaded Control.Exception.catch.
--
-- `catch` has real Haskell source in ghc-internal/src/GHC/Internal/IO.hs:
--
--   catch (IO io) handler = IO $ catch# io handler'
--     where handler' e = case fromException e of
--             Just e' -> unIO (handler e')
--             Nothing -> raiseIO# e
--
-- This used to require a host `catchB` shim. The source path now relies
-- on the `catch#` primop, the IO newtype/state bridge, and the Val-level
-- `fromException` helper for the exact `SomeException` instance.

import Control.Exception (catch, SomeException, evaluate)

handler :: SomeException -> IO String
handler _ = pure "recovered"

main :: IO ()
main = do
    r1 <- catch (evaluate (error "boom") >> pure "action-not-thrown") handler
    putStrLn r1
    r2 <- catch (pure "clean result") handler
    putStrLn r2
