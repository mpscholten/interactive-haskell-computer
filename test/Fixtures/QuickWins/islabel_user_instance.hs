-- Phase 3.5: user-defined IsLabel instance wins over the default Proxy
-- instance. `fromLabel` dispatches into the user's method body instead of
-- falling through to the built-in `VCon "Proxy" []` default.
import GHC.OverloadedLabels (fromLabel, IsLabel(..))

data Wrap = Wrap String deriving Show

instance IsLabel s Wrap where
    fromLabel = Wrap "custom"

main = do
    let w = (fromLabel #anything :: Wrap)
    print w
