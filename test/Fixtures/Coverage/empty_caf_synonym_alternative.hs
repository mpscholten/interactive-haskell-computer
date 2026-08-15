-- CAF signature is a type synonym; RHS is Alternative.empty forwarded
-- to MonadPlus.mzero (`empty = mzero`), same shape as
-- `instance Alternative (ParsecT e s m) where empty = mzero`.
-- Custom ADT: no megaparsec / Parser / ParsecT names.
import Control.Applicative
import Control.Monad (MonadPlus(..))

newtype Box e s a = Box a

instance Functor (Box e s) where
    fmap f (Box x) = Box (f x)

instance Applicative (Box e s) where
    pure = Box
    Box f <*> Box x = Box (f x)

instance Monad (Box e s) where
    Box x >>= k = k x

instance Alternative (Box e s) where
    empty = mzero
    l <|> r = l `mplus` r

instance MonadPlus (Box e s) where
    mzero = Box '?'
    _ `mplus` r = r

type Mid e s = Box e s
type Alias = Mid () ()

runAlias (Box x) = x

p :: Alias Char
p = empty

main = putStrLn [runAlias p]
