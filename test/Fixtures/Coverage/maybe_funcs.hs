-- Maybe operations: fromMaybe, maybe, catMaybes, mapMaybe
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main = do
    print (safeDiv 10 2)
    print (safeDiv 10 0)
    print (maybe 0 (+1) (Just 5))
    print (maybe 0 (+1) Nothing)
    let xs = [Just 1, Nothing, Just 3, Nothing, Just 5] :: [Maybe Int]
    -- filter and extract
    print [x | Just x <- xs]
