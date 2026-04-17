-- Chained value-level type applications: the evaluator skips each one.
const' x _ = x

main = do
    print (const' @Int @Bool 1 True)
    print (const' @(Maybe Int) @[Char] (Just 7) "ignored")
