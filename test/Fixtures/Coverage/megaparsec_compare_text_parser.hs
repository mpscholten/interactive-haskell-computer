-- Ord.compare on Text inside a Parser do-block after a bind.
-- hsxElementName does `name `Set.member` parents` after takeWhile1P.
-- T.pack (unstream `dstOff + 4`) used to wrap the `4` as fromInteger
-- at the live ParsecT do-carrier; int-spine Num.+ then died
-- (left=0 right=<ParsecT>).  Integer literals at the live carrier
-- now stay Int.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser Char
p = do
    _ <- char '<'
    let o = compare (T.pack "h1") (T.pack "h1")
    if o == EQ then pure 'x' else pure 'y'

main :: IO ()
main =
    case parse p "" (T.pack "<h1>") of
        Right c -> putStrLn [c]
        Left _  -> putStrLn "parse failed"
