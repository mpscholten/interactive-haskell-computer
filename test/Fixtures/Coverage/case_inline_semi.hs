-- Inline `case … of` with semicolon-separated alts (Haskell 2010 §2.7
-- allows @;@ as alt separator without requiring explicit braces).
-- IHP's parseFuncs uses this: `case readInt b of Just (n, "") -> Just n; _ -> Nothing`.
classify :: Maybe Int -> String
classify m = case m of Just 0 -> "zero"; Just _ -> "some"; _ -> "none"

main = do
    putStrLn (classify (Just 0))
    putStrLn (classify (Just 5))
    putStrLn (classify Nothing)
