-- Gap: `StandaloneDeriving` — `deriving instance ...` outside the data declaration. Seen in: IHP 13 files (generated model types). Ref: ihp-unsupported-scan.md.
{-# LANGUAGE StandaloneDeriving #-}

data Foo = Foo Int

deriving instance Eq Foo
deriving instance Show Foo

main = do
    print (Foo 1 == Foo 1)
    print (Foo 2)
