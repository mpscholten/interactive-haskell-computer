-- Newtype wrappers
newtype Name = Name String

newtype Age = Age Int

getName (Name s) = s
getAge (Age n) = n

main = do
    let n = Name "Alice"
    let a = Age 25
    putStrLn (getName n)
    print (getAge a)
    -- Newtype as constructor
    let names = [Name "Bob", Name "Carol", Name "Dave"]
    printNames names

printNames [] = pure ()
printNames (x:xs) = do
    putStrLn (getName x)
    printNames xs
