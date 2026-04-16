-- TupleSections: missing middle element
withBrackets = ("(", , ")")

getFst t = case t of { (x, _, _) -> x }
getSnd t = case t of { (_, y, _) -> y }
getThd t = case t of { (_, _, z) -> z }

main = do
    let t = withBrackets "hello"
    putStrLn (getFst t ++ getSnd t ++ getThd t)
