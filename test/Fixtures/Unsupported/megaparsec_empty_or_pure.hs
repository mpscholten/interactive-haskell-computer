-- Gap: empty <|> pure: needs empty CAF + <|> operand pin. altempty + altor isolates.
-- `empty <|> pure 'z'` — Alternative empty CAF as left operand of `<|>`.
-- empty CAF and `<|>` operand `pure` are independent leftovers.
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
