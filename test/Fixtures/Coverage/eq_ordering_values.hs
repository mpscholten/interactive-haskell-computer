-- Direct @Eq Ordering@ / @Eq Bool@ — the standalone
-- @deriving instance Eq Ordering@ / @Eq Bool@ in @GHC.Classes@
-- (registerOneStandaloneEq → synthStructuralEq).  This is the
-- instance the @compare _ _ == LT@ canary resolves into.  @eqIt@
-- forces the source path even with the shim present.
eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt LT LT)
    print (eqIt LT GT)
    print (eqIt EQ EQ)
    print (GT /= EQ)
    print (LT /= LT)
