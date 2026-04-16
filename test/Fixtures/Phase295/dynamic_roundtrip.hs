-- Phase 2.9.5: Dynamic round-trip (toDyn + fromDynamic).
main :: IO ()
main = do
    let dyn = toDyn typeableDict_Int (99 :: Int)
    case fromDynamic typeableDict_Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
