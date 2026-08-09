module Main (main) where

import Prelude
import qualified Language.Haskell.TH.Lib.Map as Map

main :: IO ()
main = do
    let m = Map.insert "answer" (42 :: Int)
          $ Map.insert "other" 7 Map.empty
    putStrLn $ case Map.lookup "answer" m of
        Just 42 -> "found"
        _ -> "wrong"
    putStrLn $ case Map.lookup "missing" m of
        Nothing -> "missing"
        _ -> "wrong"
