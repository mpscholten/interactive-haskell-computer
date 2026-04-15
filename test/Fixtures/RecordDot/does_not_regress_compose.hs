-- f . g (with spaces) must still parse as function composition.
addOne = (+ 1)
double = (* 2)
addThenDouble = double . addOne
main = print (addThenDouble 4)
