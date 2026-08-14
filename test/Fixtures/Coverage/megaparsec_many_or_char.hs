-- HSX `manyHsxElement <|> hsxElement` shape: many then a single char.
-- `<|>` of a many-result (already ParsecT) vs a char.
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser String
p = many (char 'x') <|> fmap (:[]) (char 'y')

main = case parse p "" (T.pack "xx<") of
  Right s -> putStrLn s
  Left _  -> putStrLn "parse failed"
