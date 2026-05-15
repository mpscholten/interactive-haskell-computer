-- Phase 2.10a removal canary: bracket / bracketOnError / bracket_ / finally /
-- onException are pure source-level resource combinators in
-- GHC/Internal/Control/Exception/Base.hs (layered on mask/catch/throwIO).
-- They must be interpreted from source, not host-shimmed. This fixture
-- asserts exact ordered stdout on BOTH the success and exception paths so a
-- broken chain cannot silently pass.
import Control.Exception

boom :: IO a
boom = throwIO (userError "boom")

main :: IO ()
main = do
    -- bracket success path: acquire, use, release (release runs after use)
    _ <- bracket
            (putStrLn "acquire" >> pure "res")
            (\_ -> putStrLn "release")
            (\r -> putStrLn ("use " ++ r) >> pure ())

    -- finally: cleanup runs even when the body throws; rethrow caught at top
    r1 <- try (finally (putStrLn "finally-body" >> boom) (putStrLn "finally-cleanup"))
            :: IO (Either SomeException ())
    case r1 of
        Left _  -> putStrLn "finally-rethrew"
        Right _ -> putStrLn "finally-no-exn"

    -- onException: cleanup runs ONLY on the exception path
    _ <- onException (putStrLn "onexc-ok-body" >> pure ()) (putStrLn "onexc-ok-cleanup-SHOULD-NOT-PRINT")
    r2 <- try (onException (putStrLn "onexc-bad-body" >> boom) (putStrLn "onexc-bad-cleanup"))
            :: IO (Either SomeException ())
    case r2 of
        Left _  -> putStrLn "onexc-rethrew"
        Right _ -> putStrLn "onexc-no-exn"

    -- bracket_ success path: before, thing, after
    _ <- bracket_ (putStrLn "b_-before") (putStrLn "b_-after") (putStrLn "b_-thing")

    -- bracket_ exception path: after still runs, exception rethrown
    r3 <- try (bracket_ (putStrLn "b_e-before") (putStrLn "b_e-after") (putStrLn "b_e-thing" >> boom))
            :: IO (Either SomeException ())
    case r3 of
        Left _  -> putStrLn "b_e-rethrew"
        Right _ -> putStrLn "b_e-no-exn"

    -- bracketOnError: release runs ONLY when the in-between throws
    _ <- bracketOnError
            (pure "ok")
            (\_ -> putStrLn "boe-ok-release-SHOULD-NOT-PRINT")
            (\_ -> putStrLn "boe-ok-thing")
    r4 <- try (bracketOnError
                (pure "bad")
                (\_ -> putStrLn "boe-bad-release")
                (\_ -> putStrLn "boe-bad-thing" >> boom))
            :: IO (Either SomeException ())
    case r4 of
        Left _  -> putStrLn "boe-rethrew"
        Right _ -> putStrLn "boe-no-exn"

    putStrLn "done"
