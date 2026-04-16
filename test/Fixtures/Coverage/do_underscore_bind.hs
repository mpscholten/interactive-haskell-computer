main :: IO ()
main = do
    _ <- pure (42 :: Int)
    putStrLn "ok"
