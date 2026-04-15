data Expr = Num Int | Add Expr Expr | Mul Expr Expr

instance Show Expr where
    show (Num n)   = show n
    show (Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
    show (Mul l r) = "(" ++ show l ++ "*" ++ show r ++ ")"

main = do
    let e = Add (Num 1) (Mul (Num 2) (Num 3))
    putStrLn (show e)
