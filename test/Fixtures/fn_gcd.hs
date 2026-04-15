-- Euclid's algorithm using `if` + `<=` to branch.
-- Since we don't yet have `-` that distinguishes which operand is
-- larger, this uses a simple structural approach: if a <= b, recurse
-- with (b-a) and a, else swap.
--
-- Equivalent Haskell:
--   gcd' a b = if a <= 0 then b else gcd' (b - (b / a) * a) a  -- needs /
--
-- Simpler 2-arg test: `sumTo a b = if a <= b then a + sumTo (a+1) b else 0`
-- (sum of ints in [a..b])
sumTo a b = if a <= b then a + sumTo (a + 1) b else 0
main = sumTo 1 10
-- 1+2+...+10 = 55
