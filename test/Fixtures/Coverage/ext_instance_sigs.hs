-- Gap: `InstanceSigs` — type signature inside an `instance` body. Seen in: IHP 13 files. Ref: ihp-unsupported-scan.md.
{-# LANGUAGE InstanceSigs #-}

class Greet a where
    greet :: a -> String

data Who = Who

instance Greet Who where
    greet :: Who -> String
    greet _ = "hi"

main = putStrLn (greet Who)
