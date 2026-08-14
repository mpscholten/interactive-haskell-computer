-- Int >= Int / compare must stay Ord Int after Network.HTTP.Date
-- (and its Data.Time / Data.Word closure) has been loaded.
--
-- Converter.adjust's `td >= aj` is the library hang leftover.  A
-- last-writer Ord instance from Time/Word used to rewrite these to
-- the wrong dictionary (derived Ord Bool / infinite adjust).  The
-- class dispatcher plus honest export of >= / compare keep Int.
import Network.HTTP.Date

main :: IO ()
main = do
    putStrLn "imported"
    let a = 239 :: Int
        b = 14 :: Int
    print (a >= b)
    print (compare (1 :: Int) (2 :: Int))
