-- OverloadedStrings Set.fromList ["h1"] :: Set Text must fromString
-- each element so Set.member (T.pack "h1") hits.  IHP.HSX.Parser does
--   parents = Set.fromList ["a", …, "h1", …] :: Set Text
--   name `Set.member` parents
-- Pre-fix the list stayed [Char]: fromList is an IsList homonym, so
-- the expected Set Text never reached the cons spine.  Defining-module
-- Set.Internal (facade Data.Set + Text still lazy-rebuilds Internal).
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set.Internal as Set
import qualified Data.Text as T

parents :: Set.Set T.Text
parents = Set.fromList ["h1"]

main :: IO ()
main = putStrLn (if Set.member (T.pack "h1") parents then "h1" else "no")
