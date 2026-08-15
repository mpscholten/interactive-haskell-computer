-- parseErrorPretty after a failed Text parse.  Isolated from
-- parseErrorTextPretty leftover (++ args=<function> "\n") and from
-- errorBundlePretty (still times out).  parseErrorPretty warms
-- Semigroup [Char] (<>) then calls parseErrorTextPretty.
import Data.Void (Void)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left e ->
            case bundleErrors e of
                err :| _ -> putStrLn (parseErrorPretty err)
        Right c -> putStrLn [c]
