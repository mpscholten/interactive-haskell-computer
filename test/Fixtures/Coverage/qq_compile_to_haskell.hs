-- quoteExp field does compileToHaskell-shaped quotation: let-bound
-- quotes plus `$var` splices, and a local String quoted by name.
-- IHP.HSX.QQ.compileToHaskell writes
--   [| applyAttributes ($element (mconcat $renderedChildren)) $stringAttributes |]
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Quote (QuasiQuoter(..))

apply el attrs = case attrs of
    [] -> el
    _  -> el
h1 x = x
pre x = x

qq :: QuasiQuoter
qq = QuasiQuoter
  { quoteExp  = \s ->
        let element = [| h1 |]
            stringAttributes = [| [] |]
        in [| apply ($element (pre s)) $stringAttributes |]
  , quotePat  = \_ -> error "quotePat: not defined"
  , quoteType = \_ -> error "quoteType: not defined"
  , quoteDec  = \_ -> error "quoteDec: not defined"
  }

main :: IO ()
main = putStrLn [qq|hello|]
