-- Composition of result-polymorphic Enum.toEnum must apply toEnum.
-- Models GHC.Internal.Unicode.generalCategory = toEnum . GC.generalCategory
-- without naming Char / GeneralCategory / isAlphaNum.
data C = A | B | C deriving (Show, Enum)

f :: Int -> Int
f _ = 1

main :: IO ()
main = print ((toEnum . f) 0 :: C)
