{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module ConstructorMetadataH98Rejected where

data Record a = Record { unRecord :: a }
data Existential a = forall x. Existential x
data Context a = Eq a => Context a
data Multiline a =
    Multiline a
data Kinded (a :: Type) = Kinded a
