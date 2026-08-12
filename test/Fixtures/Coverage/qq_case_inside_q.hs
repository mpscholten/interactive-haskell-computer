-- case/pure inside a signed Q quoteExp, without megaparsec.
-- Isolates the Either-case step of quoteHsxExpression from parseHsx.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))

caseQQ :: QuasiQuoter
caseQQ = QuasiQuoter
  { quoteExp = \s -> do
      case s of
        [] -> fail "empty"
        _  -> pure (LitE (StringL s))
  , quotePat = \_ -> error "quotePat"
  , quoteType = \_ -> error "quoteType"
  , quoteDec = \_ -> error "quoteDec"
  }

main :: IO ()
main = putStrLn [caseQQ|hello|]
