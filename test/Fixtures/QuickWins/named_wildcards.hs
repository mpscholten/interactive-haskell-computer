-- NamedWildCards: `_xs :: [Int]` etc. in type sigs parse as wildcards
-- (interpreter skips the type sig, so the name is descriptive only).
addThem :: _a -> _b -> Int
addThem x y = x + y

lengthOf :: [_elem] -> Int
lengthOf [] = 0
lengthOf (_ : xs) = 1 + lengthOf xs

main = do
    print (addThem 3 4)
    print (lengthOf [10, 20, 30, 40])
