-- Gap: soft keyword `hiding` as ordinary identifier (Haskell Report soft keywords).
-- Ref: Hs2010LexIdent.hs. Soft keyword `as` already works.
hiding = 1
main = print hiding
