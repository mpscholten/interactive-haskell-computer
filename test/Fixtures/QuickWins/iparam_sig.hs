data Ctx = Ctx Int
greet :: (?ctx :: Ctx) => String -> String
greet name = case ?ctx of Ctx n -> name ++ " (" ++ show n ++ ")"
main = let ?ctx = Ctx 42 in putStrLn (greet "hello")
