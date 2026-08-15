-- QQ leftover is field force of quoteExp on a source-loaded
-- Language.Haskell.TH.Quote.QuasiQuoter record.  Pin selector and
-- record-pattern projection; do not run the Q action (that's a
-- separate leftover).
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))

strQQ :: QuasiQuoter
strQQ = QuasiQuoter
    { quoteExp  = \s -> pure (LitE (StringL ("exp:" ++ s)))
    , quotePat  = \_ -> error "quotePat"
    , quoteType = \_ -> error "quoteType"
    , quoteDec  = \_ -> error "quoteDec"
    }

main :: IO ()
main = do
    quoteExp strQQ `seq` putStrLn "selector"
    case strQQ of
        QuasiQuoter { quoteExp = qe } ->
            qe `seq` putStrLn "pattern"
