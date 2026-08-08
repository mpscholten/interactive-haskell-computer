-- Infix operator equations whose left arg is a list or paren pattern.
-- Regression for scan: col-1 '[' / '(' must register under the op name
-- (e.g. .++), not fall through as an unbound variable.
infixr 5 .++
(.++) :: [a] -> [a] -> [a]
[] .++ ys = ys
(x:xs) .++ ys = x : (xs .++ ys)

main = print ([1,2] .++ [3 :: Int])
