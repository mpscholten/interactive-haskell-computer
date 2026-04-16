-- NamedFieldPuns on postfix record-update: `r { x }` → `r { x = x }`.
data Point = Point { x :: Int, y :: Int } deriving Show

main = do
    let x = 99
    let p = Point { x = 1, y = 2 }
    let q = p { x }
    print (case q of Point { x = xv, y = yv } -> xv + yv)
