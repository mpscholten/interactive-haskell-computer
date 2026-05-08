-- RankNTypes: nested `forall` in argument position. Parses as a
-- `TyForall` wrapper; the runtime applies `f` to values of different
-- types as in any rank-1 case.
--
-- Uses a local polymorphic identity (`myId`) instead of `Prelude.id`,
-- which is also a `Category` class method and currently routes
-- through the class dispatcher — orthogonal pre-existing limitation.
{-# LANGUAGE RankNTypes #-}

myId :: a -> a
myId x = x

applyBoth :: (forall a. a -> a) -> (Int, String) -> (Int, String)
applyBoth f (x, y) = (f x, f y)

main = print (applyBoth myId (42, "hello"))
