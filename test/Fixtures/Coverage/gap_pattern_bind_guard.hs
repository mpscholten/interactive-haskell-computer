-- Gap: guards on pattern bindings `(pat) | guard = expr` (Haskell 2010 §4.3.6).
-- Ref: Hs2010Bindings.hs 4.3.6.
(a, b) | True = (1, 2)

main = print (a + b)
