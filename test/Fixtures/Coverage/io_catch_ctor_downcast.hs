-- Warp TimeManager leftover: `handle ignore` where
--   ignore TimeoutThread = return ()
-- is applied after Val-level fromException always returns
-- `Just (SomeException inner)`. A constructor-pattern catch handler
-- must not `error` with
--   Non-exhaustive patterns in function: [[PCon "TimeoutThread" []]]
--   args=SomeException IOError …
-- against a different wrapped exception; source catch should take the
-- Nothing branch and re-raise. Custom ADT so we do not depend on Warp.
--
-- Ladder:
--   1. throwIO user SomeException, case fromException downcast
--   2. constructor-pattern handle (TimeoutThread shape) vs other exception
--   3. same handle vs IOError (socket-op shape)

import Control.Exception
import Data.List (isInfixOf)

data Tick = Tick deriving Show
instance Exception Tick

data Boom = Boom String deriving Show
instance Exception Boom

ignore Tick = putStrLn "caught-tick"

-- The leftover wraps the handler PatternMatchFail as IOError, so an
-- IOException downcast is not enough. The canary is the function-clause
-- miss on the constructor-pattern handler.
classify :: SomeException -> String
classify e
    | "Non-exhaustive patterns in function" `isInfixOf` show e = "pmf"
    | otherwise =
        case fromException e of
            Just (Boom s) -> "boom:" ++ s
            Nothing ->
                case fromException e of
                    Just Tick -> "tick"
                    Nothing ->
                        case fromException e of
                            Just (_ :: IOException) -> "ioe"
                            Nothing -> "other"

main :: IO ()
main = do
    r1 <- try (throwIO (Boom "user")) :: IO (Either SomeException ())
    case r1 of
        Left se ->
            case fromException se of
                Just Tick -> putStrLn "fe-tick"
                Nothing   -> putStrLn "fe-miss"
        Right _ -> putStrLn "fe-right"

    r2 <- try $ handle ignore $ throwIO (Boom "not-tick")
    case r2 of
        Left e  -> putStrLn ("handle-boom: " ++ classify e)
        Right _ -> putStrLn "handle-boom-swallowed"

    r3 <- try $ handle ignore $ throwIO (userError "socket-op")
    case r3 of
        Left e  -> putStrLn ("handle-ioe: " ++ classify e)
        Right _ -> putStrLn "handle-ioe-swallowed"

    -- Lambda-pattern handler (same leftover, different desugar prefix)
    r4 <- try $ handle (\Tick -> putStrLn "lam-tick") $
            throwIO (userError "socket-op-lam")
    case r4 of
        Left e  -> putStrLn ("handle-lam: " ++ classify e)
        Right _ -> putStrLn "handle-lam-swallowed"

    handle ignore $ throwIO Tick
