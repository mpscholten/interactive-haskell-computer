import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

parser :: Parser Text
parser = takeWhile1P (Just "text") (\c -> c /= '<' && c /= '>')

main :: IO ()
main =
    case runParser parser "" (Text.pack "hello<") of
        Right text -> putStrLn (Text.unpack text)
        Left _ -> putStrLn "parse failed"
