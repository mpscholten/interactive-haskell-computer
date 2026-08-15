-- Leftover bare Alternative.empty as left operand of `<|>` next to a
-- pinned result-poly `pure`.  Custom ADT + synonym carrier: same shape
-- as Alternative (ParsecT) (`empty = mzero`, `(<|>) = mplus`) but no
-- megaparsec / Parser / ParsecT names.
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
p = empty <|> pure 'z'

main = putStrLn [runAlias p]
