{-# LANGUAGE RecordWildCards #-}

data User = User { name :: String, age :: Int } deriving Show

showU :: User -> String
showU User{ name, age } = name ++ " (" ++ show age ++ ")"

showU' :: User -> String
showU' User{..} = name ++ " (" ++ show age ++ ")"

alice = User { name = "Alice", age = 30 }

main = do
    putStrLn (showU alice)
    putStrLn (showU' alice)
