-- Nested Q inside quoteExp must consume the residual
-- `String -> Q Exp` scheme from durable record-selector metadata.
-- Without that evidence the inner bind is elaborated as the wrong
-- monad (historically ParsecT / IO) instead of Q.
--
-- Reduced: a signed Q do-block with only `pure`. `location` is
-- `Q qLocation` with a circular source `instance Quasi Q`, which is a
-- separate host-Quasi issue.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..))

inner :: Q Int
inner = do
  pure 1

nestedQ :: QuasiQuoter
nestedQ = QuasiQuoter
  { quoteExp = \_ -> do
      _ <- inner
      pure (LitE (IntegerL 1))
  , quotePat = \_ -> error "quotePat"
  , quoteType = \_ -> error "quoteType"
  , quoteDec = \_ -> error "quoteDec"
  }

main :: IO ()
main = case [nestedQ|ignored|] of
  1 -> putStrLn "1"
  _ -> putStrLn "wrong"
