-- Gap: Type signature `name :: T` inside a `where` block. Seen in: hasql-1.10.3/Hasql/Connection.hs:2:30 (5/6 probed packages). Ref: hackage-parser-gaps.md (cross-cutting bucket 1).
main = print (go 5)
  where
    go :: Int -> Int
    go n = n * 2
