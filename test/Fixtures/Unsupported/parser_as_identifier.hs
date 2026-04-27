-- Gap: `as` used as an ordinary identifier (Haskell soft keyword). Seen in: lens-5.3.6/Control/Lens/Fold.hs:1:2 (`repeated f a = as where as = f a .> as`). Ref: hackage-parser-gaps.md (lens bucket 4).
firstN :: Int -> [Int] -> [Int]
firstN n xs = as
  where
    as = take n xs

main = print (firstN 3 [10, 20, 30, 40, 50])
