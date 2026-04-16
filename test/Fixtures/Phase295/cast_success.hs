-- Phase 2.9.5: cast with same type returns Just.
main :: IO ()
main = do
    let val = (42 :: Int)
    case cast typeableDict_Int typeableDict_Int val of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
