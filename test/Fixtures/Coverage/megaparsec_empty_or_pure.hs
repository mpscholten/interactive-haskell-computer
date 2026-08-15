-- `empty <|> pure 'z'` — leftover bare empty as left operand of `<|>`
-- next to a pinned result-poly `pure`.  empty CAF and the `<|>`
-- operand pin are independent leftovers; this is the combination.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = empty <|> pure 'z'

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right c -> putStrLn [c]
