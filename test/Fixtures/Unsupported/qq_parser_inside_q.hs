-- Gap: megaparsec parse inside a signed Q quoteExp hangs / does not
-- return Q Exp. After location leaves, HSX's quoteHsxExpression does
--   expression <- case parseHsx settings pos exts (cs code) of
--       Left err -> fail ...
--       Right result -> pure result
-- A raw ParsecT at thExpToExpr is forbidden; the enclosing carrier
-- must stay Q Exp. Isolated from IHP.HSX so the gap is the parser
-- action inside Q, not HSX itself.
{-# LANGUAGE QuasiQuotes #-}
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseHi :: Text -> Either (ParseErrorBundle Text Void) String
parseHi = parse (string (pack "hi") *> pure "ok") ""

hiQQ :: QuasiQuoter
hiQQ = QuasiQuoter
  { quoteExp = \s -> do
      case parseHi (pack s) of
        Left err -> fail (errorBundlePretty err)
        Right result -> pure (LitE (StringL result))
  , quotePat = \_ -> error "quotePat"
  , quoteType = \_ -> error "quoteType"
  , quoteDec = \_ -> error "quoteDec"
  }

main :: IO ()
main = putStrLn [hiQQ|hi|]
