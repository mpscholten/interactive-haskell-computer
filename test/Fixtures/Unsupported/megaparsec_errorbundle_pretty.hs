-- Gap: after Int-first delay + Set.toList/Set.map GREEN, pretty is not
-- yet printable.  parseErrorTextPretty leftover is now
--   ++  [[],[ys],  [x:xs, ys]] args=<function> "\n"
-- (showTokens singleton is GREEN `'h'`; Set.map after parse is GREEN).
-- errorBundlePretty itself still times out past that text-pretty leftover.
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left e -> putStrLn (errorBundlePretty e)
        Right c -> putStrLn [c]
