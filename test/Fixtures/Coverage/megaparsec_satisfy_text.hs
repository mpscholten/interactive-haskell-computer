-- Isolated IHP.HSX.Parser satisfy path on Text: token + isAlphaNum.
-- takeWhile1P Text is GREEN; this pins satisfy (comments use
-- `satisfy (const True)`; alphaNumChar / tag-name style uses isAlphaNum).
-- Not parseHsx whole. setPosition is not required for this isolate.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)

type Parser = Parsec Void Text

p :: Parser Char
p = satisfy isAlphaNum

main :: IO ()
main =
    case parse p "" (T.pack "h1>") of
        Left _  -> putStrLn "parse failed"
        Right c -> putStrLn [c]
