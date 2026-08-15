-- Gap: notFollowedBy after many leftover unParser <(#,#)> applied to
-- <function> on stacked combo3. No unParser name list.
-- notFollowedBy after many, then eof. many / manyTill / lookAhead are
-- GREEN; this pins the next combinator leftover candidate.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = do
    xs <- many (char 'x')
    _ <- notFollowedBy (char 'x')
    eof
    pure xs

main :: IO ()
main = case parse p "" (T.pack "xxx") of
  Left _  -> putStrLn "parse failed"
  Right s -> putStrLn s
