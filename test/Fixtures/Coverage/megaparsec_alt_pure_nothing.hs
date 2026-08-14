-- combinators.optional_ / many: (Just <$> p) <|> pure Nothing.
-- Construction-time `pure Nothing` must stay a ParsecT, not IO.
-- Same leftover as hsx_hello: unParser n s applied to cok, `(#,#)` on a function.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser (Maybe Char)
p = (Just <$> char 'x') <|> pure Nothing

main = case parse p "" (T.pack "<h1>") of
  Left _  -> putStrLn "parse failed"
  Right (Just c) -> putStrLn ['J', c]
  Right Nothing  -> putStrLn "N"
