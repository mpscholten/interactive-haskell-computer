module ProviderHidden (Gamma(..), CH) where
data Gamma = Gamma
class CH a where
    pick :: a
