-- Multi-binding layout-`let` with the first binding inline with `let`.
-- Regression for IHP's `let tn = tableName @model\n    cols = …\n in …`
-- pattern: the RHS of the first binding must stop at the next binding's
-- column instead of greedily eating it as an argument.
main = print (go 6)

go n = let x = n * 2
           y = x + 1
       in x + y
