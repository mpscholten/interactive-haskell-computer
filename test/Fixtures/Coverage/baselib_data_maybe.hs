-- Data.Maybe: fromMaybe, catMaybes, mapMaybe, isJust, isNothing, maybe
--
-- Complements the existing `maybe_funcs.hs` by covering the four
-- query/elimination helpers (fromMaybe, catMaybes, mapMaybe, isJust/isNothing)
-- explicitly imported from `Data.Maybe`.
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust, isNothing, maybe)

main :: IO ()
main = do
    print (fromMaybe 0 (Just 42 :: Maybe Int))
    print (fromMaybe 0 (Nothing :: Maybe Int))
    print (catMaybes [Just 1, Nothing, Just 3, Nothing, Just 5 :: Maybe Int])
    print (mapMaybe (\x -> if x > 2 then Just (x * 10) else Nothing) [1, 2, 3, 4 :: Int])
    print (isJust    (Just 1 :: Maybe Int))
    print (isJust    (Nothing :: Maybe Int))
    print (isNothing (Just 1 :: Maybe Int))
    print (isNothing (Nothing :: Maybe Int))
    print (maybe 99 (+ 1) (Just 5 :: Maybe Int))
    print (maybe 99 (+ 1) (Nothing :: Maybe Int))
