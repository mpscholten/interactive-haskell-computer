-- Regression: 'Semigroup.(<>)' on strings.  Source-loaded source code
-- (warp's HTTP response builders, blaze's html builder, etc.) reaches
-- for @<>@ as a bare 'EVar', so the env must bind it directly rather
-- than relying on whole-program elaboration.  IHC binds @<>@ to a
-- 'sappendDispatch' that picks an instance based on the LHS tag, with
-- a fast path for cons-list / 'VStr' strings.
main :: IO ()
main = do
    let a = "hello"
    let b = ", "
    let c = "world"
    putStrLn (a <> b <> c)
    putStrLn (mconcat [a, b, c])
