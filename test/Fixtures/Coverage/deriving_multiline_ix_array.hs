import Data.Array (Array, Ix, listArray, (!))

-- Companion to deriving_multiline_clause: a derived `Ix` from a multi-line
-- deriving clause must be captured so an Array indexed by the type dispatches
-- to the derived instance rather than the host `Ix Int` shim (which would
-- throw "Ix Int.index: non-Int index" on a constructor index).
data Color = Red | Green | Blue
    deriving
        ( Show, Eq, Ord, Enum, Bounded
        , Ix
        )

arr :: Array Color String
arr = listArray (Red, Blue) ["r", "g", "b"]

main :: IO ()
main = putStrLn (arr ! Green)
