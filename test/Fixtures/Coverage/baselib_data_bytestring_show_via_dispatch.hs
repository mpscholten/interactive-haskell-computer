-- `show (BS.pack ...)` reached via the Show class dispatcher rather
-- than `print`. This distinguishes the source-loaded `instance Show
-- ByteString` body from the eqVals/showDispatch VCon "BS" path.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    putStrLn (show (BS.pack [104, 105]))
    putStrLn (show BS.empty)
    putStrLn (show (BS.pack [33]))
