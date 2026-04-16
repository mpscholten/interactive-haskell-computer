import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    let bs = BL.pack "42"
    let r = A.decode bs :: Maybe Int
    print r
    putStrLn "decode ok"
