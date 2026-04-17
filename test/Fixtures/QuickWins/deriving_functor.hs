-- DeriveFunctor: synthesize @instance Functor T@ from a
--   @deriving (Functor, ...)@ clause for any single-parameter user ADT.
--
-- For each ctor we emit @fmap f (C v1 v2 ...) = C (role v1) (role v2) ...@
-- where fields that reference the last type variable are transformed by
-- @f@ and fields whose type contains the variable recursively invoke
-- @fmap f@ via the class registry.
data Box a = Box a deriving (Functor, Show)
data Pair a = Pair a a deriving (Functor, Show)

main :: IO ()
main = do
    print (fmap (+1) (Box 10))
    print (fmap (*2) (Pair 3 4))
