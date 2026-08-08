-- Pattern / as-pattern bindings in a where-block must not let their
-- RHS parse past the next sibling binding at the same column.
--
-- bsb-http-chunked's chunkedTransferEncoding uses:
--
--   go innerStep (BufferRange op ope)
--     | … = fillWithBuildStep … doneH …
--     where
--       !brInner@(BufferRange opInner _) = BufferRange (…) (…)
--       doneH opInner' _ = wrapChunk opInner' …
--
-- Before the fix, parseWherePatBind / as-pattern used the outer
-- ctxMinCol, so @BufferRange (op+2) (ope-1)@ greedily ate @doneH@ as
-- extra arguments — leaving @doneH@ unbound at runtime (busy-spin /
-- error on the warp HTTP/1.1 chunked response path).

data BR = BR Int Int

fill step doneH fullH br = doneH 5 0

outer k =
    go start
  where
    start = 0
    go step (BR op ope)
      | outRemaining < 3 = k (-1)
      | otherwise        = fill step doneH fullH brInner
      where
        outRemaining = ope - op
        !brInner@(BR opInner _) = BR (op + 2) (ope - 1)
        doneH opInner' _ =
            k (opInner' + opInner + outRemaining)
        fullH _ minSz _ = k minSz

main :: IO ()
main = print (outer id (BR 0 10))
-- outRemaining=10, opInner=2, doneH 5 0 -> 5+2+10 = 17
