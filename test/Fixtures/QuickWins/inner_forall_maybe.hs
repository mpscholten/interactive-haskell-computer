-- Rank-2 forall in a function argument position, similar to `withRunInIO`.
-- Parser must accept `(forall a. Maybe a -> Maybe a)` without choking.
runLike :: (forall a. Maybe a -> Maybe a) -> Maybe Int
runLike f = f (Just 1)

myId :: a -> a
myId x = x

main = print (runLike myId)
