-- Constructor-pattern do-bind: `W n <- action`.
-- Sibling of `CInt n <- peek p` without a CInt name list — a local
-- unary wrapper must take the same path (ETyApp pin + match).
data Box a = Box a

main :: IO ()
main = do
    Box n <- return (Box (7 :: Int))
    print n
    Box m <- return (Box (Just (3 :: Int)))
    case m of
        Just 3 -> putStrLn "just"
        _      -> putStrLn "bad"
