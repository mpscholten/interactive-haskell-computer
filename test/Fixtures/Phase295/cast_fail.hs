-- Phase 2.9.5: cast with different types returns Nothing.
{-# LANGUAGE TypeApplications #-}
import Data.Typeable (cast)

main :: IO ()
main = do
    let val = (42 :: Int)
    case cast @Int @Bool val of
        Just _  -> putStrLn "Just"
        Nothing -> putStrLn "Nothing"
