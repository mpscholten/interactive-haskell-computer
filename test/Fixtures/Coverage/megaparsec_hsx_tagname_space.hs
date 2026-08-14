-- HSX hsxElementName: takeWhile1P isAlphaNum, then space (takeWhileP).
-- No unless/fail — just the two combinators parseHsx uses on "h1>".
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T
import qualified Data.Char as Char

type Parser = Parsec Void T.Text

p :: Parser T.Text
p = do
    name <- takeWhile1P (Just "identifier") Char.isAlphaNum
    _ <- takeWhileP (Just "white space") Char.isSpace
    pure name

main = case parse p "" (T.pack "h1>Hello") of
  Right t -> putStrLn (T.unpack t)
  Left _  -> putStrLn "parse failed"
