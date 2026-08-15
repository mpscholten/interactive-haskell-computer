-- HSX leftover: lookAhead peeks a token, then consume it.
-- Isolated as `lookAhead (char 'h') *> char 'h'`.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = lookAhead (char 'h') *> char 'h'

main = case parse p "" (T.pack "hello") of
  Left _  -> putStrLn "parse failed"
  Right c -> putStrLn [c]
