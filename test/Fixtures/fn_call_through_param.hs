-- Calling a function with a parameter passed as the argument.
-- `addOne x = x + 1`, `twice x = addOne x + addOne x`
-- main: twice 10 = 11 + 11 = 22
addOne x = x + 1
twice x = addOne x + addOne x
main = twice 10
