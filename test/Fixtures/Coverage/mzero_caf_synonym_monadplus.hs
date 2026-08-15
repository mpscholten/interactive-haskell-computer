-- CAF signature is a type synonym; RHS is MonadPlus.mzero.
-- Same shape as `instance MonadPlus (ParsecT e s m) where mzero = pZero`.
-- Custom ADT: no megaparsec / Parser / ParsecT names.
-- empty CAF (empty = mzero) is GREEN; this is the un-forwarded mzero CAF.
import Control.Monad (MonadPlus(..))

newtype Box e s a = Box a

instance Functor (Box e s) where
    fmap f (Box x) = Box (f x)

instance Applicative (Box e s) where
    pure = Box
    Box f <*> Box x = Box (f x)

instance Monad (Box e s) where
    Box x >>= k = k x

instance MonadPlus (Box e s) where
    mzero = Box '?'
    _ `mplus` r = r

type Mid e s = Box e s
type Alias = Mid () ()

runAlias (Box x) = x

p :: Alias Char
p = mzero

main = putStrLn [runAlias p]
