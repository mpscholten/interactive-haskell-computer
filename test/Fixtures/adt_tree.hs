data Tree = Leaf | Node Int Tree Tree

size t = case t of
    Leaf       -> 0
    Node _ l r -> 1 + size l + size r

main = print (size (Node 1 (Node 2 Leaf Leaf) Leaf))
