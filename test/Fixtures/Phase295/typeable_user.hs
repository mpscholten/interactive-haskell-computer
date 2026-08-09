-- Phase 2.9.5: Typeable/Dynamic works alongside a user-defined data type.
{-# LANGUAGE TypeApplications #-}
import Data.Dynamic (fromDynamic, toDyn)

data Box = Box Int

main :: IO ()
main = do
    let dyn = toDyn (42 :: Int)
    case fromDynamic @Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
