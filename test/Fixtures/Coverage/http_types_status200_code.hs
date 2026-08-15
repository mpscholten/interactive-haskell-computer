-- Unqualified reduction of warp hello's status200 use.
-- statusCode is the Status record selector; "OK" is the
-- OverloadedStrings reason phrase (construction already works).
import Network.HTTP.Types (status200, statusCode)

main :: IO ()
main = print (statusCode status200)
