module ProviderAll (Alpha(..), CA(..)) where
data Alpha = Alpha
class CA a where
    pick :: a
