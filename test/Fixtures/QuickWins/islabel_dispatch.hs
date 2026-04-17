-- Phase 3.5: IsLabel dispatch
-- A #label used in a Proxy context (IHP's default instance) should behave
-- like a Proxy. The evaluator treats `Proxy` pattern-matches against a
-- VLabel as transparent, so `case #email of Proxy -> ...` works, and the
-- explicit `fromLabel` builtin produces a `VCon "Proxy" []`.
import Data.Proxy
import GHC.OverloadedLabels (fromLabel)

main = do
    -- (1) Parenthesized type ascription is accepted; default value is #email.
    let p = (#email :: Proxy "email")
    print p
    -- (2) Explicit fromLabel produces Proxy; pattern-match confirms.
    let q = fromLabel #email
    case q of
        Proxy -> putStrLn "fromLabel-yielded Proxy matched"
    -- (3) Pattern-match Proxy against a raw VLabel — transparent default.
    case #firstName of
        Proxy -> putStrLn "raw VLabel matched Proxy"
