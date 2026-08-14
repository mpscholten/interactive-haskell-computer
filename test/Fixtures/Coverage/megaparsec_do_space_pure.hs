-- do { space; pure () } after a possibly-empty takeWhileP.
-- `pure ()` must stay ParsecT, not IO.  space itself is
-- void $ takeWhileP (Just "white space") isSpace.
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

parser :: Parser ()
parser = do
    space
    pure ()

main :: IO ()
main =
    case parse parser "" (T.pack "<h1>") of
        Left _  -> putStrLn "parse failed"
        Right _ -> putStrLn "ok"
