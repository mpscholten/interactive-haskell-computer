-- StandaloneDeriving: `deriving instance Cls Typ` at the top level synthesizes
-- the same dictionary as inline `deriving (Cls)` on the data declaration.
-- Roadmap: IHP Tier-3 item #13 (what-is-still-needed-groovy-lobster.md).
{-# LANGUAGE StandaloneDeriving #-}

data Foo a = Foo a

deriving instance Show a => Show (Foo a)
deriving instance Eq a => Eq (Foo a)

main :: IO ()
main = do
    let f = Foo (42 :: Int)
    print f
    print (f == Foo 42)
