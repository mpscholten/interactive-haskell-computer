-- `.` composition via Pratt parser.
addThenDouble = (\x -> x * 2) . (\x -> x + 3)
main = print (addThenDouble 4)
