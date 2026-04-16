data Tree a = Bin !Int !a !(Tree a) !(Tree a) | Tip

empty :: Tree Int
empty = Tip

main :: IO ()
main = case empty of
    Tip -> putStrLn "ok"
    _   -> putStrLn "unexpected"
