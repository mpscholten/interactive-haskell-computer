-- some (char 'a') on Text.  many (char _) is already GREEN
-- (megaparsec_many_char); some = liftM2 (:) p (many p).
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = some (char 'a')

run :: String -> String -> IO ()
run label input = case parse p "" (T.pack input) of
  Left _  -> putStrLn (label ++ " fail")
  Right s -> putStrLn (label ++ ":" ++ s)

main :: IO ()
main = do
  run "empty" ""
  run "aaa" "aaa"
  run "aaX" "aaX"
