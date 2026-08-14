-- HSX leftover after takeWhile1P isAlphaNum: space =
--   void $ takeWhileP (Just "white space") isSpace
-- takeWhileP allows an empty match (takeWhile_); takeWhile1P does not.
-- Isolated from IHP.HSX so the gap is the token, not HSX itself.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

type Parser = Parsec Void Text

parser :: Parser Text
parser = takeWhileP (Just "ws") isSpace

main :: IO ()
main =
    case parse parser "" (T.pack "  hi") of
        Left _  -> putStrLn "err"
        Right t -> putStrLn (T.unpack t)
