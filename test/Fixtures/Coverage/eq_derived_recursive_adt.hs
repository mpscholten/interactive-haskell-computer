-- @deriving Eq@ structural synthesis (registerDerivedEqInstances /
-- synthStructuralEq) on a recursive sum+product ADT, driven through a
-- polymorphic @eqIt@ so the elaborator emits @ETypedMethod "Eq" "=="@
-- and the SOURCE/synth path fires even while the @eqDispatch@ shim
-- still exists (so this locks correctness before the Stage-2 removal).
data Tree = Leaf Int | Node Tree Tree deriving (Eq, Show)

eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt (Node (Leaf 1) (Leaf 2)) (Node (Leaf 1) (Leaf 2)))
    print (eqIt (Node (Leaf 1) (Leaf 2)) (Node (Leaf 1) (Leaf 9)))
    print (eqIt (Leaf 5) (Leaf 5))
    print (eqIt (Leaf 5) (Node (Leaf 1) (Leaf 1)))
