import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    let x = A.encode (Just (1 :: Int))
    BL.putStrLn x
    putStrLn "encode maybe int ok"
