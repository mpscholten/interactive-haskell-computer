-- Gap: custom-ADT synonym carrier: pure <|> pure not pinned. GREEN in altor isolate.
-- Result-poly `pure` as both operands of a same-carrier combinator.
-- Custom ADT + synonym carrier: no megaparsec / Parser / ParsecT names.
import Control.Applicative

newtype Box e s a = Box a

instance Functor (Box e s) where
    fmap f (Box x) = Box (f x)

instance Applicative (Box e s) where
    pure = Box
    Box f <*> Box x = Box (f x)

instance Alternative (Box e s) where
    empty = Box '?'
    Box x <|> _ = Box x

type Mid e s = Box e s
type Alias = Mid () ()

runAlias (Box x) = x

p :: Alias Char
p = pure 'a' <|> pure 'b'

main = putStrLn [runAlias p]
