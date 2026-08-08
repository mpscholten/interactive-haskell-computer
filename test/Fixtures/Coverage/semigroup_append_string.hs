-- Regression: 'Semigroup.(<>)' on strings.  Source-loaded source code
-- (warp's HTTP response builders, blaze's html builder, etc.) reaches
-- for @<>@ as a bare 'EVar', so IHC must resolve it through the
-- source-loaded class-method dispatcher rather than a host shim.
main :: IO ()
main = do
    let a = "hello"
    let b = ", "
    let c = "world"
    putStrLn (a <> b <> c)
    putStrLn (mconcat [a, b, c])
