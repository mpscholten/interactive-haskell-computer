-- A.2 — Haskell Report §3.17.3: irrefutable patterns defer match
-- failure until a bound variable is FORCED.  Here we apply f to
-- Nothing AND read x — that triggers the deferred match against
-- Nothing, which fails: stdout is empty before the host exception
-- fires, and the message contains "Irrefutable pattern failed".
import Control.Exception (try, SomeException, evaluate)

f :: Maybe Int -> Int
f = \ ~(Just x) -> x

main :: IO ()
main = do
    -- Forcing the result of `f Nothing` should fire the deferred
    -- match.  In the current ihc, host ErrorCall isn't bridged
    -- through `try @SomeException`, so the program crashes via
    -- stderr.  We stay defensive: if try ever does catch it, we
    -- print "deferred"; otherwise the host runtime prints the tag.
    r <- try (evaluate (f Nothing)) :: IO (Either SomeException Int)
    case r of
        Left _  -> putStrLn "deferred"
        Right v -> putStrLn ("not deferred (got " <> show v <> ")")
