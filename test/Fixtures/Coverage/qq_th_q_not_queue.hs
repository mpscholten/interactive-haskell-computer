-- TH's newtype Q is not in the constructor-type registry (rank-n
-- record field `forall m. Quasi m => m a` fails closed).  A later
-- `data Queue e = Q …` (Data.Sequence.Internal.Sorting, or this
-- local stand-in) then becomes the unique registered Q.
--
-- Elaborating `instance Monad Q` must not steal that Queue constructor
-- type.  If it does, unification fails, >>= is a placeholder, and
-- result-poly >>= yields a ParsecT that thExpToExpr cannot decode.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Lit(..), location)
import Text.Megaparsec ()

data Queue e = Q e ()

hiQQ :: QuasiQuoter
hiQQ = QuasiQuoter
  { quoteExp = \_ -> do
      _ <- location
      pure (LitE (StringL "ok"))
  , quotePat = \_ -> error "quotePat"
  , quoteType = \_ -> error "quoteType"
  , quoteDec = \_ -> error "quoteDec"
  }

main :: IO ()
main = putStrLn [hiQQ|ignored|]
