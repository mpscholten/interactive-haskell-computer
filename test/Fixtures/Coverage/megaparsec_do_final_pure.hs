import Data.Void (Void)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text.Text

parser :: Parser Char
parser = do
    c <- char 'x'
    pure c

main :: IO ()
main =
    case parse parser "" (Text.pack "x") of
        Left _ -> putStrLn "parse failed"
        Right c -> putStrLn [c]
