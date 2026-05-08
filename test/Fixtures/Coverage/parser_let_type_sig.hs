-- Gap: Type signature `name :: T` inside a layout `let` block. Seen in: IHP/ModelSupport.hs:7:16. Ref: ihp-parser-gaps.md (bucket 9).
main = do
    let go :: Int -> Int
        go n = n * 2
    print (go 5)
