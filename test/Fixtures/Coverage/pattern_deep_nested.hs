data Tree = Leaf | Node Tree Int Tree

depth Leaf = 0
depth (Node l _ r) =
    let dl = depth l
        dr = depth r
    in 1 + (if dl > dr then dl else dr)

main = do
    let t = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 Leaf)
    print (depth t)
