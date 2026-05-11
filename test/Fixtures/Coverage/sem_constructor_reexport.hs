-- Gap: Constructor re-export `(..)` across module boundaries. Seen in: aeson/Data/Aeson.hs, hspec-expectations. Ref: aeson-dryrun-findings.md + hspec-dryrun-findings.md.
module Main where

-- Explicitly re-export via (..) should bring both `Just` and `Nothing` into scope.
import Data.Maybe (Maybe(..))

main = case Just (42 :: Int) of
    Just n  -> print n
    Nothing -> putStrLn "nothing"
