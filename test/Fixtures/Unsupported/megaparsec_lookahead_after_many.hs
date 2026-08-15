-- Gap: lookAhead after many leftover unParser <(#,#)> applied to
-- <function> on stacked combo3. No unParser name list.
-- lookAhead after many. many / manyTill satisfy are GREEN; this is the
-- next combinator leftover candidate (parseHsx itself does not use it).
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = do
    xs <- many (char 'x')
    _ <- lookAhead (char '>')
    _ <- char '>'
    pure xs

main :: IO ()
main = case parse p "" (T.pack "xxx>") of
  Left _  -> putStrLn "parse failed"
  Right s -> putStrLn s
