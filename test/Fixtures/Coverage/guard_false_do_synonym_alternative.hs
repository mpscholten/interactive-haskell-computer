-- guard False in a do-block must stay on the do-carrier (`empty`),
-- not leftover-return IO / Identity / a function.  Source guard is
--   guard True  = pure ()
--   guard False = empty
-- and Alternative empty forwards to mzero (`empty = mzero`).
-- Custom ADT + synonym: no megaparsec / Parser / ParsecT names.
import Control.Applicative
import Control.Monad (MonadPlus(..), guard)

data Box e s a = Fail | Ok a

instance Functor (Box e s) where
    fmap _ Fail = Fail
    fmap f (Ok x) = Ok (f x)

instance Applicative (Box e s) where
    pure = Ok
    Fail <*> _ = Fail
    _ <*> Fail = Fail
    Ok f <*> Ok x = Ok (f x)

instance Monad (Box e s) where
    Fail >>= _ = Fail
    Ok x >>= k = k x

instance Alternative (Box e s) where
    empty = mzero
    Fail <|> r = r
    l <|> _ = l

instance MonadPlus (Box e s) where
    mzero = Fail
    Fail `mplus` r = r
    l `mplus` _ = l

type Mid e s = Box e s
type Alias = Mid () ()

runAlias Fail = "FAIL"
runAlias (Ok x) = [x]

p :: Alias Char
p = do
    guard False
    pure '!'

main = putStrLn (runAlias p)
