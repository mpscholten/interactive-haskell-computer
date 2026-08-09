-- Phase 2.9.5: TypeRep equality via cast.
-- cast with same type returns Just; different type returns Nothing.
-- We test using Dynamic round-trip.
{-# LANGUAGE TypeApplications #-}
import Data.Dynamic (fromDynamic, toDyn)

main :: IO ()
main = do
    let dyn = toDyn (42 :: Int)
    case fromDynamic @Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
