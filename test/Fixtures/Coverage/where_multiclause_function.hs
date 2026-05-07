-- Regression: a where-bound function with multiple pattern-clauses must
-- preserve ALL clauses.  This shape comes up in Data.ByteString.Lazy
-- (foldlChunks: `go !a Empty = a` / `go !a (Chunk c cs) = ...`) which
-- warp's HTTP1 path bottoms into when computing request body length.
data Lst = LNil | LCons Int Lst

mySum :: Lst -> Int
mySum = go 0
  where go !a LNil         = a
        go !a (LCons x xs) = go (a + x) xs

main :: IO ()
main = do
    let val = LCons 1 (LCons 2 (LCons 3 LNil))
    print (mySum val)
    print (mySum LNil)
