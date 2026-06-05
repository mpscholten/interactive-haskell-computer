-- Regression for source-loaded Char/Int primitive representation overlap.
--
-- `chr` returns a raw Char# value represented as VChar.  Some source-loaded
-- base paths immediately consume that payload through an Int#/I#-shaped match
-- while IHC is still running without type-directed coercion.
import Data.Char (chr, ord)

main :: IO ()
main = do
    print (ord (chr 0))
    print (compare (chr 0) (chr 1))
    print (compare (chr 0) (chr 0))
