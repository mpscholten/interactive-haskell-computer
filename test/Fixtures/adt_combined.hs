data Maybe a = Nothing | Just a
data Tree   = Leaf | Node Int Tree Tree

fromMaybe def m = case m of
    Nothing -> def
    Just x  -> x

size t = case t of
    Leaf       -> 0
    Node _ l r -> 1 + size l + size r

main = do
    print (fromMaybe 99 Nothing)
    print (fromMaybe 0 (Just 42))
    print (size (Node 1 (Node 2 Leaf Leaf) Leaf))
