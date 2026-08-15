-- OverloadedStrings list literal at expected [Text] must fromString
-- each element.  ["h1"] :: [Text] is a cons of a String, not a
-- String; without the expected-type push the elements stay [Char]
-- and T.unpack dies matching Text arr off len on "h1".
-- IHP.HSX.Parser parents = Set.fromList ["h1", …] :: Set Text is the
-- same list-of-string-lits shape under a unary F T annotation.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

main :: IO ()
main = do
    print (map T.unpack (["h1"] :: [T.Text]))
    print (map T.unpack (["a", "div", "h1", "span"] :: [T.Text]))
