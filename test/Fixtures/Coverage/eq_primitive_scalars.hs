-- Primitive Eq instances (Int / Char / Double / Bool) via a
-- polymorphic @eqIt@ so the source-loaded instance methods fire:
-- @instance Eq Int@ (@==#@), @Eq Char@ (@eqChar#@), @Eq Double@
-- (@==##@), standalone @deriving instance Eq Bool@.  Result is Bool
-- (no float formatting), so the golden is representation-stable.
eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt (3 :: Int) 3)
    print (eqIt (3 :: Int) 4)
    print (eqIt 'x' 'x')
    print (eqIt 'x' 'y')
    print (eqIt (1.5 :: Double) 1.5)
    print (eqIt (1.5 :: Double) 2.5)
    print (eqIt True True)
    print (eqIt True False)
