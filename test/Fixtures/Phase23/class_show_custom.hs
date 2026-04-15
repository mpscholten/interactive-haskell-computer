-- User-defined Show instance via class registry.
data Shape = Circle | Square | Triangle

instance Show Shape where
    show Circle   = "Circle"
    show Square   = "Square"
    show Triangle = "Triangle"

main = do
    putStrLn (show Circle)
    putStrLn (show Square)
    putStrLn (show Triangle)
