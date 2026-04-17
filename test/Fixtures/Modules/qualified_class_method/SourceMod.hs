module SourceMod
    ( myNeg
    , myMax
    ) where
import qualified Prelude as P

myNeg :: P.Int -> P.Int
myNeg x = P.negate x

myMax :: P.Int
myMax = P.maxBound
