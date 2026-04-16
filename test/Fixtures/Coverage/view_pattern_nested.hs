-- ViewPatterns: fallthrough with multiple alts
triple x = x * 3

describeTriple x = case x of
    (triple -> 0) -> "zero"
    (triple -> n) -> show n

main = do
    putStrLn (describeTriple 0)
    putStrLn (describeTriple 7)
