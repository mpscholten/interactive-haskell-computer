-- Phase 2.9.5: TypeRep equality via cast.
-- cast with same type returns Just; different type returns Nothing.
-- We test using Dynamic round-trip.
main :: IO ()
main = do
    let dyn = toDyn typeableDict_Int (42 :: Int)
    case fromDynamic typeableDict_Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
