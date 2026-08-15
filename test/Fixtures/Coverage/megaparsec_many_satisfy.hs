-- many (satisfy isAlphaNum) on Text.  many (char _) / some (char _) are
-- GREEN; HSX children / attrs use many / manyTill of satisfy-shaped
-- parsers (rawAttribute, comments, spliced-node leaves).
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)

type Parser = Parsec Void Text

p :: Parser String
p = many (satisfy isAlphaNum)

run :: String -> String -> IO ()
run label input = case parse p "" (T.pack input) of
  Left _  -> putStrLn (label ++ " fail")
  Right s -> putStrLn (label ++ ":" ++ s)

main :: IO ()
main = do
  run "empty" ""
  run "h1" "h1"
  run "h1>" "h1>"
