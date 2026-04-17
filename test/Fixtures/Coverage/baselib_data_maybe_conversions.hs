-- Data.Maybe: fromJust, maybeToList
--
-- Covers the extraction / list-conversion helpers.
-- NOTE: `listToMaybe` currently fails with an unbound `foldr` at the
-- time the Maybe source module is compiled; not exercised here.
import Data.Maybe (fromJust, maybeToList)

main :: IO ()
main = do
    print (fromJust (Just 42 :: Maybe Int))
    print (fromJust (Just "hello" :: Maybe String))
    print (maybeToList (Just 5 :: Maybe Int))
    print (maybeToList (Nothing :: Maybe Int))
