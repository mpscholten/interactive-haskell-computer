-- Tuple equality recurses through the per-component source-loaded Eq
-- instances (@deriving instance Eq (a,b)@ / @Eq (a,b,c)@ in
-- @GHC.Classes@).  Mixed component types (Int/Char/Bool) exercise the
-- recursive field dispatch.
main :: IO ()
main = do
    print (((1 :: Int), 'a') == (1, 'a'))
    print (((1 :: Int), 'a') == (1, 'b'))
    print (((1 :: Int), 'a', True) == (1, 'a', True))
    print (((1 :: Int), 'a', True) == (1, 'a', False))
    print (((1 :: Int), 'a') /= (2, 'a'))
