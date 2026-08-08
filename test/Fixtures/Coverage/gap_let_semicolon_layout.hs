-- Gap: multi-binding let with semicolons but no braces (`let x = 1; y = 2 in e`).
-- Braced form already works. Ref: Hs2010ExprCtl multi-binding layout let.
main = print (let x = 1; y = 2 in x + y)
