-- Same as io_bracket_family but with an explicit named import list, exercising
-- the Control.Exception.<name> FQN-alias resolution path after the host shims
-- are removed (source-loaded from GHC/Internal/Control/Exception/Base.hs).
import Control.Exception (bracket, bracket_, bracketOnError, finally, onException, try, SomeException, evaluate)
import GHC.Internal.IO (throwIO)

boom :: IO a
boom = throwIO (userError "boom")

main :: IO ()
main = do
    _ <- bracket
            (putStrLn "acquire" >> pure "res")
            (\_ -> putStrLn "release")
            (\r -> putStrLn ("use " ++ r) >> pure ())

    r1 <- try (finally (putStrLn "finally-body" >> boom) (putStrLn "finally-cleanup"))
            :: IO (Either SomeException ())
    case r1 of
        Left _  -> putStrLn "finally-rethrew"
        Right _ -> putStrLn "finally-no-exn"

    _ <- onException (putStrLn "onexc-ok-body" >> pure ()) (putStrLn "onexc-ok-cleanup-SHOULD-NOT-PRINT")
    r2 <- try (onException (putStrLn "onexc-bad-body" >> boom) (putStrLn "onexc-bad-cleanup"))
            :: IO (Either SomeException ())
    case r2 of
        Left _  -> putStrLn "onexc-rethrew"
        Right _ -> putStrLn "onexc-no-exn"

    _ <- bracket_ (putStrLn "b_-before") (putStrLn "b_-after") (putStrLn "b_-thing")

    r3 <- try (bracket_ (putStrLn "b_e-before") (putStrLn "b_e-after") (putStrLn "b_e-thing" >> boom))
            :: IO (Either SomeException ())
    case r3 of
        Left _  -> putStrLn "b_e-rethrew"
        Right _ -> putStrLn "b_e-no-exn"

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
