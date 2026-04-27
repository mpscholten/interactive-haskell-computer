-- Gap: Pattern guard `| p <- expr` in function equation. Seen in: lens/Internal/FieldTH.hs:1:10, IHP/RouterSupport.hs:1:13. Ref: ihp-parser-gaps.md bucket 11.
lookupDouble :: Int -> [(Int, Int)] -> Maybe Int
lookupDouble k xs
    | Just v <- lookup k xs = Just (v * 2)
    | otherwise             = Nothing

main = do
    print (lookupDouble 2 [(1, 10), (2, 20)])
    print (lookupDouble 3 [(1, 10), (2, 20)])
