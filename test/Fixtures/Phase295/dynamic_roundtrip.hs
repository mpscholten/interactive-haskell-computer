-- Phase 2.9.5: Dynamic round-trip (toDyn + fromDynamic).
{-# LANGUAGE TypeApplications #-}
import Data.Dynamic (fromDynamic, toDyn)

main :: IO ()
main = do
    let dyn = toDyn (99 :: Int)
    case fromDynamic @Int dyn of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
