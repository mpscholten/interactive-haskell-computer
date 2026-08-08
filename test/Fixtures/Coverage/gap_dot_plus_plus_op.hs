-- Gap: three-char dot-operator `.++` must lex as one token (not TkDot + ++).
-- Seen in: servant-server Context.hs type/value uses of (.++).
infixr 5 .++
(.++) :: [a] -> [a] -> [a]
(.++) [] ys = ys
(.++) (x:xs) ys = x : ((.++) xs ys)

main = print ((.++) [1, 2] [3 :: Int])
