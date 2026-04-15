-- Phase 3.5: (#email, "hi") — the IHP filterWhere pattern
main = do
    let pair = (#email, "hi")
    case pair of
        (lbl, val) -> do
            print lbl
            putStrLn val
