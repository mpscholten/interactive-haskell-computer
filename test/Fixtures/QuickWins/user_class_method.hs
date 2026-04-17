-- User-defined class whose methods bind as top-level names via
-- 'scanClassDecls'. The instance body provides both clauses of `bar`
-- and the class dispatcher resolves `bar True` / `bar False` by
-- the runtime type tag of the argument.
class Foo a where
    bar :: a -> Int

instance Foo Bool where
    bar True  = 1
    bar False = 0

main = print (bar True + bar False)
