-- Pattern matching forces to WHNF
data Tree = Leaf | Node Tree Int Tree

insert x Leaf = Node Leaf x Leaf
insert x (Node l v r)
    | x < v    = Node (insert x l) v r
    | x > v    = Node l v (insert x r)
    | otherwise = Node l v r

toList Leaf = []
toList (Node l v r) = toList l ++ [v] ++ toList r

fromList [] t = t
fromList (x:xs) t = fromList xs (insert x t)

main = do
    let t = fromList [5, 3, 7, 1, 4, 6, 8] Leaf
    print (toList t)
