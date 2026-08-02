-- EQuasiQuote foundation: expand a user-defined QuasiQuoter end-to-end.
--
-- Pipeline under test:
--   parse  [strQQ|…|]  → EQuasiQuote "strQQ" bodyBytes
--   eval   lookup strQQ, project $fldProj$quoteExp, apply body String
--   runQ   (IO/Q action) → TH LitE (StringL …) Val
--   decode thExpToExpr → ELit/cons-list Expr → putStrLn
--
-- Uses the real 'QuasiQuoter' record from source-loaded
-- Language.Haskell.TH.Quote, and the synthetic LitE/StringL
-- constructors from IHC.TH — no ihp-hsx / blaze required.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))

strQQ :: QuasiQuoter
strQQ = QuasiQuoter
  { quoteExp  = \s -> pure (LitE (StringL s))
  , quotePat  = \_ -> error "quotePat: not defined"
  , quoteType = \_ -> error "quoteType: not defined"
  , quoteDec  = \_ -> error "quoteDec: not defined"
  }

main :: IO ()
main = putStrLn [strQQ|hello world|]
