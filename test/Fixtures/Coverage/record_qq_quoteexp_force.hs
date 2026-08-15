-- QQ leftover is field force of quoteExp on a QuasiQuoter-shaped
-- record.  Custom ADT so the fixture does not depend on
-- template-haskell.  Construction `QQ { quoteExp = ... }` plus
-- selector application must yield the field, not a leftover function.
data QQ = QQ
    { quoteExp  :: String -> String
    , quotePat  :: String -> String
    , quoteType :: String -> String
    , quoteDec  :: String -> String
    }

strQQ :: QQ
strQQ = QQ
    { quoteExp  = \s -> "exp:" ++ s
    , quotePat  = \_ -> "pat"
    , quoteType = \_ -> "type"
    , quoteDec  = \_ -> "dec"
    }

forceQuoteExp :: QQ -> String -> String
forceQuoteExp q = quoteExp q

main :: IO ()
main = do
    putStrLn (quoteExp strQQ "hi")
    putStrLn (forceQuoteExp strQQ "ok")
    putStrLn (quotePat strQQ "x")
