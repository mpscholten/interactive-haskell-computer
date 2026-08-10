-- A source QuasiQuoter returns the real template-haskell Q newtype, rather
-- than an already-lowered host IO action.  The antiquotation boundary must
-- choose IO for the stored rank-polymorphic Quasi action and consume exactly
-- that one Q layer before decoding LitE.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q(..), Exp(..), Lit(..))

sourceQ :: QuasiQuoter
sourceQ = QuasiQuoter
  { quoteExp  = \s -> Q (pure (LitE (StringL s)))
  , quotePat  = \_ -> error "quotePat: not defined"
  , quoteType = \_ -> error "quoteType: not defined"
  , quoteDec  = \_ -> error "quoteDec: not defined"
  }

main :: IO ()
main = putStrLn [sourceQ|source Q|]
