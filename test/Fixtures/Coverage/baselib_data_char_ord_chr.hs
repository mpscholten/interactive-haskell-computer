-- Data.Char: ord, chr
--
-- Only ord/chr are covered here. `isDigit`/`isAlpha`/`isUpper` currently
-- trip a LoopException (infinite forcing loop), `toLower`/`toUpper` trip
-- a `Prelude.read: no parse` when the Unicode tables are driven from
-- source, and `C.toSimpleUpperCase` is not yet resolved. ord/chr live
-- directly in `GHC.Base` / `Data.Char` as simple Int-backed conversions,
-- which already work.
import Data.Char (ord, chr)

main :: IO ()
main = do
    print (ord 'A')
    print (ord '0')
    print (chr 65)
    print (chr 97)
    print (chr (ord 'Z' + 1))
