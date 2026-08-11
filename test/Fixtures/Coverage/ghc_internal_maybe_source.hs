import GHC.Internal.Maybe (Maybe(..))
import Prelude (IO, pure)

main :: IO Int
main = pure (case Just 19 of Just value -> value; Nothing -> 0)
