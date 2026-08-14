-- Data.Char.isAlphaNum / generalCategory.  GHC.Internal.Unicode defines
--   generalCategory = toEnum . GC.generalCategory
-- so composition must apply result-polymorphic Enum.toEnum (tag 1 ->
-- LowercaseLetter).  HSX hsxElementName is takeWhile1P … isAlphaNum.
import Data.Char (isAlphaNum, generalCategory, GeneralCategory(LowercaseLetter))

main :: IO ()
main = do
    print (isAlphaNum 'h')
    print (isAlphaNum '1')
    print (generalCategory 'h' == LowercaseLetter)
