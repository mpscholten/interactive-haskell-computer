-- Mixed comma-separated boolean + pattern guards (lens Deque check style).
-- | cond, p <- e, (a,b) <- e2 = …
-- Also: pure pattern-guard chains and bool-then-pattern mixes.

lookupScaled :: Int -> [(Int, Int)] -> Maybe Int
lookupScaled k xs
  | k > 0, Just v <- lookup k xs = Just (v * 2)
  | otherwise = Nothing

-- Multiple pattern guards after a bool condition (lens Deque `check`):
-- | lf > …, i <- …, (f',f'') <- … = …
splitHalf :: [a] -> ([a], [a])
splitHalf xs
  | n > 0, i <- div n 2, (lo, hi) <- splitAt i xs = (lo, hi)
  | otherwise = ([], xs)
  where
    n = length xs

-- Bool, pattern, bool again
pick :: Int -> Maybe Int -> String
pick n m
  | n > 0, Just x <- m, x > 10 = "big"
  | n > 0, Just x <- m = "small"
  | otherwise = "none"

main = do
    print (lookupScaled 2 [(1, 10), (2, 20)])
    print (lookupScaled 0 [(1, 10), (2, 20)])
    print (lookupScaled 3 [(1, 10), (2, 20)])
    print (splitHalf [1, 2, 3, 4])
    print (splitHalf ([] :: [Int]))
    putStrLn (pick 1 (Just 20))
    putStrLn (pick 1 (Just 5))
    putStrLn (pick 0 (Just 20))
    putStrLn (pick 1 Nothing)
