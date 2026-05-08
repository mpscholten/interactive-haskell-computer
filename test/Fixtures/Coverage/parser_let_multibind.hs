-- Gap: Multi-binding layout `let` without braces. Seen in: conduit-1.3.6.1/Conduit/Internal/Conduit.hs:3:25 (also hasql, lens, servant-server). Ref: hackage-parser-gaps.md (cross-cutting bucket 2).
main = do
    let x = 1
        y = 2
        z = 3
    print (x + y + z)
