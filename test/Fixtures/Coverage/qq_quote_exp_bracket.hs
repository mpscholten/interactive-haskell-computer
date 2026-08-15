-- QuasiQuoter.quoteExp field whose body is a quotation of the parameter.
-- HSX compileToHaskell writes `[| Html5.preEscapedText value |]` the same
-- way: a local value is quoted and must be Lifted, not emitted as VarE.
-- Previous leftover (PAP stored in the record field, then `seq`) is GREEN
-- in qq_quote_exp_pap / qq_toy_string.
{-# LANGUAGE QuasiQuotes #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))

qq :: QuasiQuoter
qq = QuasiQuoter
  { quoteExp  = \s -> [| s |]
  , quotePat  = \_ -> error "quotePat: not defined"
  , quoteType = \_ -> error "quoteType: not defined"
  , quoteDec  = \_ -> error "quoteDec: not defined"
  }

main :: IO ()
main = putStrLn [qq|hello|]
