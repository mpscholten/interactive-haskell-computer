-- Gap: Wildcard `_` on LHS of a `let` binding. Seen in: conduit-1.3.6.1/Conduit/Internal/Conduit.hs:2:14. Ref: hackage-parser-gaps.md.
main = do
    let _ = "ignored side binding"
    putStrLn "hello"
