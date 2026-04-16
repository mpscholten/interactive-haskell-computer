-- Phase 2.9.5: Typeable/Dynamic works alongside a user-defined data type.
data Box = Box Int

main :: IO ()
main = do
    let dyn = toDyn typeableDict_Int (42 :: Int)
    case fromDynamic typeableDict_Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
