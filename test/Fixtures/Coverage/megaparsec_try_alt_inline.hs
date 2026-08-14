-- Inline `try p <|> q` (hsxElement = try comment <|> try selfClosing <|> normal).
-- Where-bound / annotated try as <|> operand is GREEN; inline try is the leftover
-- (unParser p / pTry). The char that matches should print.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = try (char 'x') <|> char '<'

main = case parse p "" (T.pack "<h1>") of
  Left _  -> putStrLn "parse failed"
  Right c -> putStrLn [c]
