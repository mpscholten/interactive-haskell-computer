-- Gap: bare wildcard as left arg of infix op `_ .++ ys = ys`.
infixr 5 .++
_ .++ ys = ys

main = print ([1, 2] .++ [3 :: Int])
