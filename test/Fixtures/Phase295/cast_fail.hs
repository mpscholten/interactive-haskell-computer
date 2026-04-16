-- Phase 2.9.5: cast with different types returns Nothing.
main :: IO ()
main = do
    let val = (42 :: Int)
    case cast typeableDict_Int typeableDict_Bool val of
        Just _  -> putStrLn "Just"
        Nothing -> putStrLn "Nothing"
