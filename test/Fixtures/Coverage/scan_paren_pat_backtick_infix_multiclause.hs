module Main where

data Maybe' a = Nothing' | Just' a

(Nothing') `pickM` (y) = y
(Just' x)  `pickM` _   = Just' x

showM :: Maybe' Int -> String
showM Nothing' = "Nothing"
showM (Just' n) = "Just " ++ show n

main :: IO ()
main = do
  putStrLn (showM (Nothing' `pickM` Just' 1))
  putStrLn (showM (Just' 9 `pickM` Just' 1))
