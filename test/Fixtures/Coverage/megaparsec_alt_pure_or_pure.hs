-- `pure 'a' <|> pure 'b' :: Parser Char`.  Both operands are
-- result-poly `pure`; first arg of `<|>` is not a dispatchable ParsecT
-- (unlike `char 'x' <|> char 'y'`).
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = pure 'a' <|> pure 'b'

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right c -> putStrLn [c]
