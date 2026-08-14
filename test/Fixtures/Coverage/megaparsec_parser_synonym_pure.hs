-- Standalone CAF `pure` whose signature is the Parser synonym.
-- `p :: Parser Char; p = pure 'z'` must run as ParsecT, not IO.
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

type Parser = Parsec Void Text

p :: Parser Char
p = pure 'z'

main = case parse p "" (T.pack "a") of
    Right c -> putStrLn [c]
    Left _  -> putStrLn "parse failed"
