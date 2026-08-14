-- many (char 'x') on "" and on "xxx".
-- optional_ / (Just <$> p) <|> pure Nothing is already GREEN.
-- many itself succeeds; do not call Foldable.length here (that leftover
-- is megaparsec_import_length_list.hs: last-writer NonEmpty.length).
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = many (char 'x')

run :: String -> String -> IO ()
run label input = case parse p "" (T.pack input) of
  Left _  -> putStrLn (label ++ " fail")
  Right s -> putStrLn (label ++ ":" ++ s)

main :: IO ()
main = do
  run "empty" ""
  run "xxx" "xxx"
