-- HSX hsxElementName is takeWhile1P isAlphaNum then `unless valid (fail …)`
-- then space (takeWhileP isSpace).  Source unless is
--   unless p s = if p then pure () else s
-- Unannotated `pure ()` defaulted to IO, so ParsecT bind did
--   unParser (k x) s' cok
-- on a (# State#, () #) function — the hsx_hello leftover after
-- takeWhileP isSpace itself was GREEN.
import Text.Megaparsec
import Data.Void
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char

type Parser = Parsec Void Text

parser :: Parser Text
parser = do
    name <- takeWhile1P (Just "identifier") Char.isAlphaNum
    unless True (fail "bad tag")
    _ <- takeWhileP (Just "white space") Char.isSpace
    pure name

main :: IO ()
main =
    case parse parser "" (T.pack "h1>Hello") of
        Left _  -> putStrLn "parse failed"
        Right t -> putStrLn (T.unpack t)
