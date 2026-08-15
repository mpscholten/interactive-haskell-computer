-- Warp TimeManager.registerKillThread: registerTimeout callback throwTo
-- must interrupt the target thread's threadDelay / takeMVar, and
-- `handle ignore` with `ignore Tick = return ()` must catch it.
-- Isolated: GHC.Event.registerTimeout + throwTo Tick + constructor-
-- pattern handle. Custom ADT so we do not depend on Warp.
-- See warp_ghc_event_timer and ghc_event_unregister_timeout.
import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Exception
import Data.List (isInfixOf)
import GHC.Event (getSystemTimerManager, registerTimeout)
import System.IO (hFlush, stdout)

data Tick = Tick deriving Show
instance Exception Tick

classify :: SomeException -> String
classify e
    | "Non-exhaustive patterns in function" `isInfixOf` show e = "pmf"
    | "Non-exhaustive patterns in lambda" `isInfixOf` show e = "pmf-lam"
    | otherwise =
        case fromException e of
            Just Tick -> "tick"
            Nothing ->
                case fromException e of
                    Just (_ :: IOException) -> "ioe"
                    Nothing -> "other"

ignore Tick = putStrLn "caught-tick" >> hFlush stdout

main :: IO ()
main = do
    mgr <- getSystemTimerManager
    tid <- myThreadId
    -- Force the ThreadId in the parent so the child cannot re-run
    -- a lazy myThreadId bind (that would throwTo self and hang).
    let !_ = length (show tid)
    r <- try $ handle ignore $ do
        _ <- registerTimeout mgr 50000 (throwTo tid Tick)
        threadDelay 200000
        putStrLn "no-timeout" >> hFlush stdout
    case r of
        Left e  -> putStrLn ("escaped: " ++ classify e) >> hFlush stdout
        Right _ -> putStrLn "done" >> hFlush stdout
