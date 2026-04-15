data Maybe a = Nothing | Just a

fromMaybe def m = case m of
    Nothing -> def
    Just x  -> x

main = do
    print (fromMaybe 99 Nothing)
    print (fromMaybe 0 (Just 42))
