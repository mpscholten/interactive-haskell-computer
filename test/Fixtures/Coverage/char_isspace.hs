-- Data.Char.isSpace.  ASCII spaces use Word ord-arithmetic, not
-- generalCategory (that path is only for codepoints > 0x377).
-- HSX space = void $ takeWhileP (Just "white space") isSpace.
import Data.Char (isSpace)

main :: IO ()
main = do
    print (isSpace ' ')
    print (isSpace 'h')
