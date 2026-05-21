-- Runtime lowering for the stdlib shape `(#.) _f = coerce` must preserve
-- the left operand as a newtype wrapper/accessor in IHC's tagged runtime.

import Data.Coerce (coerce)

newtype Any = Any Bool

getAny :: Any -> Bool
getAny (Any b) = b

(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce

main :: IO ()
main = print (getAny ((Any #. not) False))
