-- Gap: `FunctionalDependencies` — `class C a b | a -> b where`. Seen in: IHP/Record.hs:77 (`class SetField name model value | field model -> value`), IHP 12 files. Ref: ihp-unsupported-scan.md.
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

class Convert a b | a -> b where
    convert :: a -> b

instance Convert Int String where
    convert = show

main = putStrLn (convert (42 :: Int))
