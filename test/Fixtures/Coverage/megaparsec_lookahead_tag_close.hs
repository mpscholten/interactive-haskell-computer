-- HSX parser lookAheads tag close / comments. Tiny Parsec Void Text:
-- lookAhead (string "</") must not consume; the same tokens are then
-- read. Isolates lookAhead leftover after runIdentity / unParser.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Text
p = do
    _ <- lookAhead (string (T.pack "</"))
    string (T.pack "</")

main = case parse p "" (T.pack "</h1>") of
  Left _  -> putStrLn "LEFT"
  Right s -> putStrLn (T.unpack s)
