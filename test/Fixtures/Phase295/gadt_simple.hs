-- Phase 2.9.5: GADT form — constructors registered from 'where' block.
-- The constructors MkInt and MkBool are GADT-style; at runtime they are
-- just ordinary arity-1 constructors (the return type is ignored).
data Expr where
    MkInt  :: Int -> Expr
    MkBool :: Bool -> Expr

getInt :: Expr -> Int
getInt (MkInt n) = n
getInt _         = 0

main :: IO ()
main = do
    let x = MkInt 42
    print (getInt x)
