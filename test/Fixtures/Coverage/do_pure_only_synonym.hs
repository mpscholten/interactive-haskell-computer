-- Single-statement `do { pure x }` CAF. Direct `p = pure 'z'` is GREEN
-- (`pure_caf_synonym_applicative`). A lone EDo is not pinned: wrap must
-- stamp the do so eval uses monadicCarrierFromType, not the IO default.
-- Custom ADT + synonym carrier: no megaparsec / Parser / ParsecT names.
newtype Box e s a = Box a

instance Functor (Box e s) where
    fmap f (Box x) = Box (f x)

instance Applicative (Box e s) where
    pure = Box
    Box f <*> Box x = Box (f x)

type Mid e s = Box e s
type Alias = Mid () ()

runAlias (Box x) = x

p :: Alias Char
p = do
    pure 'M'

main = putStrLn [runAlias p]
