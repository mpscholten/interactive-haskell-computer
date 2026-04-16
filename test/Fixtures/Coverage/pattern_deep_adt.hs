-- Deep nested constructor matching
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Neg x)   = negate (eval x)

-- deep nesting
main = do
    let e1 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
    let e2 = Neg (Add (Lit 10) (Neg (Lit 3)))
    let e3 = Mul (Add (Lit 2) (Lit 3)) (Neg (Lit 4))
    print (eval e1)
    print (eval e2)
    print (eval e3)
