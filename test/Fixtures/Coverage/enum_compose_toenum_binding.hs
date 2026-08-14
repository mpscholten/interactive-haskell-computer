-- Same as enum_compose_toenum, but the composition lives in a signed
-- top-level binding — the GHC.Internal.Unicode.generalCategory shape
-- (`toEnum . f` with result type only on the binding, not the call).
data C = A | B | C deriving (Show, Enum)

f :: Int -> Int
f _ = 1

g :: Int -> C
g = toEnum . f

main :: IO ()
main = print (g 0)
