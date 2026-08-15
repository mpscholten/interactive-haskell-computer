-- parseErrorTextPretty after a failed char parse.  Uses
-- showErrorItem (Proxy :: Proxy s) with no runtime payload, then
-- Set.map.  Pre-fix: lastDispatchTagRef was ShareInput Text (wrapper
-- take1_) so VisualStream.showTokens leftover-returned; Set.map
-- collected <function> and messageItemsPretty did ++ <function> "\n".
-- Structural: record the user stream (unwrap ShareInput/NoShareInput)
-- and only for Stream-family classes.  No pretty / Set name list.
-- errorBundlePretty still hangs in reachOffset (separate slice).
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
