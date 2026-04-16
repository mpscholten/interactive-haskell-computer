module Helper (join, surround) where

join :: String -> [String] -> String
join _   []     = ""
join _   [x]    = x
join sep (x:xs) = x ++ sep ++ join sep xs

surround :: String -> String -> String -> String
surround open close s = open ++ s ++ close
