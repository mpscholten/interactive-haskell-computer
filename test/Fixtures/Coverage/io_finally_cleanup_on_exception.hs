-- Regression canary: source-loaded finally / onException / bracket_ /
-- bracketOnError MUST run their cleanup/release action on the EXCEPTION
-- path.
--
-- The host `catch` backing source-loaded `onException` (`catchB` in
-- IHC.Builtins) forced its action thunk to WHNF *before* installing the
-- exception handler.  In the Val model an IO action's WHNF can already
-- run side effects: the `restore` wrapper `mask` hands to
-- finally/bracket/onException is `unblock`/`unsafeUnmask`, whose source
-- `unsafeUnmask (IO io) = IO $ unmaskAsyncExceptions# io` deconstructs
-- the `IO` newtype with a `case`, and the evaluator eagerly runs a `VIO`
-- scrutinee.  So the body ran (and threw) during `force aT`, *outside*
-- the `try`, and the exception escaped the handler — cleanup was
-- silently skipped.  `catch` must evaluate its action *inside* the
-- protected region.  Unblocks PR #166 (bracket-family shim removal) on
-- top of #171 (try/handle source-load).
--
-- Every combinator is wrapped in `try` (deterministic golden stdout, and
-- avoids an unrelated pre-existing bare-`finally`-then-continue
-- truncation that is out of scope here).  Exact ordered stdout so a
-- broken chain cannot silently pass.
import Control.Exception

boom :: IO a
boom = throwIO (userError "boom")

main :: IO ()
main = do
    r1 <- try (finally (putStrLn "f-body" >> boom) (putStrLn "f-clean"))
            :: IO (Either SomeException ())
    putStrLn (case r1 of Left _ -> "f-rethrew"; Right _ -> "f-noexn")

    r2 <- try (onException (putStrLn "o-body" >> boom) (putStrLn "o-clean"))
            :: IO (Either SomeException ())
    putStrLn (case r2 of Left _ -> "o-rethrew"; Right _ -> "o-noexn")

    r3 <- try (bracket_ (putStrLn "b-acq") (putStrLn "b-rel")
                         (putStrLn "b-use" >> boom))
            :: IO (Either SomeException ())
    putStrLn (case r3 of Left _ -> "b-rethrew"; Right _ -> "b-noexn")

    r4 <- try (bracketOnError (pure "h")
                              (\_ -> putStrLn "boe-rel")
                              (\_ -> putStrLn "boe-use" >> boom))
            :: IO (Either SomeException ())
    putStrLn (case r4 of Left _ -> "boe-rethrew"; Right _ -> "boe-noexn")

    putStrLn "done"
