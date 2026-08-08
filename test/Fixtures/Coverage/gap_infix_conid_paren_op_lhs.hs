-- servant-server style: ConId left-arg plus paren-pat left-arg infix clauses.
-- Empty .++ a already works via handleConIdInfixLhs; (a :. as) .++ b needs
-- the paren-pattern symbolic-infix path.
data Ctx a = Empty | a :. Ctx a
  deriving Show

infixr 5 .++
Empty .++ a = a
(a :. as) .++ b = a :. (as .++ b)

main = print ((1 :. Empty) .++ (2 :. Empty :: Ctx Int))
