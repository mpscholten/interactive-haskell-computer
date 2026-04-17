-- ApplicativeDo: when a do-block's binds are independent and the final
-- statement is `pure e`, the parser rewrites it to
--     fmap (\xs -> e) a1 <*> a2 <*> ...
-- instead of a >>= chain.  Semantically identical to the monadic form,
-- but signals pipelineable structure to backends that care (e.g. hasql
-- generated row decoders, IHP.FetchPipelined).
--
-- We verify the rewrite runs by using IO actions plus `pure`: the inner
-- `do { x <- pure 1; y <- pure 2; pure (x, y) }` hits the applicative
-- path (all binds independent, trailing `pure`).  The outer do-block
-- contains a SLet/SBind/SExpr mix, so it stays on the classical monadic
-- chain.
main = do
    pair <- do
        x <- pure (1 :: Int)
        y <- pure (2 :: Int)
        pure (x, y)
    print pair

    -- Dependent case: y's RHS references x, so the independence check
    -- fails and the classical >>= chain is emitted.
    bumped <- do
        x <- pure (5 :: Int)
        y <- pure (x + 1)
        pure y
    print bumped
