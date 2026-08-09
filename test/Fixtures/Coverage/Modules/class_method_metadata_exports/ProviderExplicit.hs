module ProviderExplicit (Beta(..), CB(pick)) where
data Beta = Beta
class CB a where
    pick :: a
