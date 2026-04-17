-- Data.Tuple: swap, fst, snd, curry, uncurry
--
-- Exercises all five functions in `Data.Tuple`. `fst`/`snd` are also
-- re-exported through `Prelude`, but here we import them qualified
-- through `Data.Tuple` to ensure the re-export chain resolves.
import Data.Tuple (swap, fst, snd, curry, uncurry)

main :: IO ()
main = do
    print (swap (1 :: Int, 'a'))
    print (fst (10 :: Int, 20 :: Int))
    print (snd (10 :: Int, 20 :: Int))
    print (curry fst 5 (7 :: Int))
    print (uncurry (+) (3, 4 :: Int))
