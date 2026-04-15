infixl 5 `myop`
myop x y = x * 10 + y
main = print (1 `myop` 2 `myop` 3)
