-- PatternSignatures in do-binds: `n :: T <- action`.
-- network's getSocketOption is:
--   getSocketOption s so = do
--       n :: CInt <- getSockOpt s so
--       return $ fromIntegral n
-- IHC treated `n :: CInt <- …` as an expression type annotation
-- (SExpr), never installed the binder, then `fromIntegral n` died
-- with `IHC.Eval: unbound variable n`.
main :: IO ()
main = do
    n :: Int <- return 42
    case n of
        42 -> putStrLn "ok"
        _  -> putStrLn "bad"
    m :: Maybe Int <- return (Just 7)
    case m of
        Just 7 -> putStrLn "just"
        _      -> putStrLn "bad-maybe"
