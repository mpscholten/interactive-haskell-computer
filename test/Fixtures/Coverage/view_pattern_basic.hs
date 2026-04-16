-- ViewPatterns: basic usage
-- (f -> p) applies f to the scrutinee and then matches p
double x = x * 2

doubled x = case x of
    (double -> n) -> n

addOne x = x + 1

incremented x = case x of
    (addOne -> n) -> n

main = do
    print (doubled 21)
    print (incremented 5)
