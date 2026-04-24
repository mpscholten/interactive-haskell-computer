-- Applied-constructor pattern on the LHS of a let-binding, as in
-- IHP's `let QueryBuilder sq = getQueryBuilder …` — parseTopPat collects
-- `Wrap n` into a single PCon rather than stopping at the bare
-- constructor name.
data Wrap = Wrap Int

unwrap (Wrap n) = n

main = print (let Wrap n = Wrap 41 in n + 1)
