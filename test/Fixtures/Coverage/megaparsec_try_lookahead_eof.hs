-- parseHsx: space; try comment <|> tag; space; eof.
-- Comments / tag-close are lookAheaded then consumed:
--   try (lookAhead (string "<!--") >> string "<!--") <|> string "<"
-- lookAhead alone is GREEN; this is the try + lookAhead + eof combo.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Text
p = do
    space
    node <- try (lookAhead (string (T.pack "<!--")) >> string (T.pack "<!--"))
        <|> string (T.pack "<")
    space
    eof
    pure node

main = case parse p "" (T.pack "<") of
  Left _  -> putStrLn "LEFT"
  Right s -> putStrLn (T.unpack s)
