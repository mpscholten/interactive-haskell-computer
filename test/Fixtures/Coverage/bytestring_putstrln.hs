-- Data.ByteString.Char8.putStrLn is a local ByteString writer
-- (hPutStrLn stdout), not System.IO.putStrLn.  Word8 pack avoids
-- Char8.pack.  Golden: hi
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = C8.putStrLn (S.pack [104, 105])
