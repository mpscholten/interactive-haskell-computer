-- Phase 2.9.5: cast with same type returns Just.
{-# LANGUAGE TypeApplications #-}
import Data.Typeable (cast)

main :: IO ()
main = do
    let val = (42 :: Int)
    case cast @Int @Int val of
        Just n  -> print (n :: Int)
        Nothing -> putStrLn "Nothing"
