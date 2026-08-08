-- | @\\@ as a symbolic operator (Data.Set difference style).
-- Single @\@ remains lambda; two backslashes are an infix op.
-- Mirrors lens FieldTH/PrismTH: @Set.\\ fixedTypeVars@.

infixr 5 \\
(\\) :: [a] -> [a] -> [a]
[]     \\ _  = []
(x:xs) \\ ys = if x `elem` ys then xs \\ ys else x : (xs \\ ys)

main :: IO ()
main = print ([1, 2, 3, 4] \\ [2, 4])
