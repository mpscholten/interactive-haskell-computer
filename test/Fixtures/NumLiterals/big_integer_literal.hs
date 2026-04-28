-- A.3 — Haskell Report §3.2: numeric literals are polymorphic, but
-- ihc previously demoted every literal to 'Int64' at parse time
-- (`LInt (fromInteger n)`).  A literal exceeding Int64 range silently
-- truncated.  After A.3 the parser keeps out-of-range literals as
-- 'LInteger Integer' and evaluates them to 'VInteger', so the printed
-- representation matches the source decimal exactly.
module Main where

main :: IO ()
main = do
    print 12345678901234567890123456789
    print 100000000000000000000
