-- parseErrorTextPretty after a Text parse.  Pre-fix leftover was
-- (++) args=<function> "\n": Foldable.toList on :| (NE.sortWith in
-- bundleErrors) overwrote lastDispatchTag, so VisualStream.showTokens
-- recovered :| instead of Text and leftover-returned a function into
-- messageItemsPretty's (<> "\n").  Isolated from errorBundlePretty.
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
                err :| _ -> putStrLn (parseErrorTextPretty err)
        Right c -> putStrLn [c]
