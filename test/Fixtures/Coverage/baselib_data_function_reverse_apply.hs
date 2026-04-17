-- Data.Function: `&` (reverse application)
--
-- Covers the pipe-style operator.
-- NOTE: `on` is not in scope yet at runtime (unbound variable `on`), so
-- it's not covered by this fixture.
import Data.Function ((&))

main :: IO ()
main = do
    print ((5 :: Int) & (+ 1) & (* 2))
    print ((10 :: Int) & negate & (+ 100))
    print ((100 :: Int) & subtract 7 & (* 3))
