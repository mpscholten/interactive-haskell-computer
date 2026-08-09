{-# LANGUAGE TypeApplications #-}

import Data.Dynamic (fromDyn, fromDynamic, toDyn)
import Prelude (Bool, IO, Int, Maybe, print)

main :: IO ()
main = do
    print (fromDynamic @Int (toDyn (42 :: Int)))
    print (fromDynamic @Bool (toDyn (42 :: Int)))
    print (fromDyn @Int (toDyn (42 :: Int)) (0 :: Int))
