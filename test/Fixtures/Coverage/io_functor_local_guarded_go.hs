data NonEmpty a = a :| [a]

mk :: IO (NonEmpty Int)
mk = do
    a <- pure (1 :: Int)
    (a :|) <$> go (0 :: Int)
  where
    go n
        | n == 0 = return []
        | otherwise = do
            xs <- go (n - 1)
            return (n : xs)

main :: IO ()
main = do
    xs <- mk
    case xs of
        a :| as -> do
            print a
            print as
