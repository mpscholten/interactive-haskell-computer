-- Gap: `where` clause attached to a binding inside a `let` block. Seen in: conduit-1.3.6.1/Conduit/Internal/Conduit.hs (where-in-let sub-case). Ref: hackage-parser-gaps.md.
main = do
    let result = compute 10
          where
            compute x = x * x
    print result
