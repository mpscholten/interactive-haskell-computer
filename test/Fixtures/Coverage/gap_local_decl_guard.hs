-- Gap: local-decl guards `| let y = e` in function equations (Haskell 2010 §4.4.3).
-- Ref: Hs2010Bindings.hs 4.4.3; Hs2010ExprCtl case local-decl guards.
f x
  | let y = x * 2
  , y > 0 = y
  | otherwise = 0

main = print (f 3)
