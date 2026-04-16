-- Pattern binding in let: let (a, b) = expr in body
swap p =
    let (x, y) = p
    in (y, x)

main = print (swap (1, 2))
