{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Typeable ((:~:)(Refl), eqT)
import Prelude (Bool(..), IO, Int, putStrLn)

main :: IO ()
main = do
    case eqT @Int @Int of
        Just Refl -> putStrLn "same"
        Nothing -> putStrLn "different"
    case eqT @Int @Bool of
        Just Refl -> putStrLn "same"
        Nothing -> putStrLn "different"
