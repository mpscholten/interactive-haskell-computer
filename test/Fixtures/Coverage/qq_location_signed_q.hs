-- Nested Q inside quoteExp that binds `location` (Q qLocation) and
-- projects loc_filename through the owner-scoped Loc selector scheme.
-- Without Quasi-method leaves, qLocation is value-directed and the
-- enclosing Q Exp is lost before thExpToExpr.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), location, loc_filename)

locQQ :: QuasiQuoter
locQQ = QuasiQuoter
  { quoteExp = \_ -> do
      loc <- location
      pure (LitE (StringL (loc_filename loc)))
  , quotePat = \_ -> error "quotePat"
  , quoteType = \_ -> error "quoteType"
  , quoteDec = \_ -> error "quoteDec"
  }

main :: IO ()
main = putStrLn [locQQ|ignored|]
