-- Source-loaded Num / Integral / Fractional dispatch on Int, Float,
-- Double.  Pre-graduation, +, -, *, /, mod, div lived in baseEnv as
-- type-polymorphic shims (Builtins.hs old TODO 2.6 block).  After
-- removal, they route through the class-method dispatcher to the
-- instance bodies in GHC/Internal/{Num,Real,Float}.hs, bottoming on
--   +# / -# / *# / modInt# / divInt#       (Int via I# unwrap)
--   plusFloat# / ... / divideFloat#        (Float via F# unwrap)
--   +## / -## / *## / /##                  (Double via D# unwrap)
main :: IO ()
main = do
    -- Int via Num Int / Integral Int instance dispatch
    print (3 + 4 :: Int)
    print (10 - 3 :: Int)
    print (6 * 7 :: Int)
    print (17 `mod` 5 :: Int)
    print (17 `div` 5 :: Int)
    -- Float via Num Float / Fractional Float instance dispatch
    print (3.0 + 4.0 :: Float)
    print (10.0 - 3.0 :: Float)
    print (6.0 * 7.0 :: Float)
    print (10.0 / 4.0 :: Float)
    -- Double via Num Double / Fractional Double instance dispatch
    print (3.0 + 4.0 :: Double)
    print (10.0 - 3.0 :: Double)
    print (6.0 * 7.0 :: Double)
    print (10.0 / 4.0 :: Double)
