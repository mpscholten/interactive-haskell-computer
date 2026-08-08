-- Minimal megaparsec on Text: single-char parse (Stream take1_).
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = char 'h'

main = case parse p "" (T.pack "hello") of
  Left e  -> putStrLn ("err: " ++ errorBundlePretty e)
  Right c -> putStrLn ("ok: " ++ [c])
