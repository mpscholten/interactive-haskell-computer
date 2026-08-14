-- CAF signature is a type synonym of a partially-applied synonym.
-- Same hole as `p :: Parser Char; p = pure 'z'` (Parser = Parsec Void Text
-- = ParsecT e s Identity).  After synonym expansion the carrier is the
-- constructor head, not the synonym.  Custom ADT: no megaparsec names.
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
p = pure 'z'

main = putStrLn [runAlias p]
