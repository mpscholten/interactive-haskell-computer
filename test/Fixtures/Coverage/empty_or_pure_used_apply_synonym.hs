-- empty <|> leftover on a synonym carrier as used-apply (not CAF).
-- CAF `p = empty <|> pure 'z'` is GREEN (`empty_or_pure_synonym`).
-- The same operands inside a function body never get the arity-0
-- result-poly pin, so leftover empty <|> leftover pure does not
-- dispatch.  Custom ADT + synonym: no megaparsec / Parser / ParsecT.
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

use :: Char -> Alias Char
use _ = empty <|> pure 'z'

main = putStrLn [runAlias (use 'x')]
