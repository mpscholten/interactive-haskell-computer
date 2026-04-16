data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | IfZ Expr Expr Expr

eval :: Expr -> Int
eval e = case e of
    Lit n       -> n
    Add a b     -> eval a + eval b
    Mul a b     -> eval a * eval b
    Neg a       -> negate (eval a)
    IfZ c t f   -> if eval c == 0 then eval t else eval f

showExpr :: Expr -> String
showExpr e = case e of
    Lit n       -> show n
    Add a b     -> "(" ++ showExpr a ++ " + " ++ showExpr b ++ ")"
    Mul a b     -> "(" ++ showExpr a ++ " * " ++ showExpr b ++ ")"
    Neg a       -> "(-" ++ showExpr a ++ ")"
    IfZ c t f   -> "ifz(" ++ showExpr c ++ ", " ++ showExpr t ++ ", " ++ showExpr f ++ ")"

run :: Expr -> IO ()
run e = putStrLn (showExpr e ++ " = " ++ show (eval e))

main :: IO ()
main = do
    run (Lit 42)
    run (Add (Lit 3) (Lit 4))
    run (Mul (Add (Lit 2) (Lit 3)) (Lit 5))
    run (Neg (Lit 7))
    run (IfZ (Lit 0) (Lit 100) (Lit 200))
    run (IfZ (Lit 1) (Lit 100) (Lit 200))
    run (Add (Mul (Lit 6) (Lit 7)) (Neg (Lit 2)))
