import qualified Data.Aeson as A

main :: IO ()
main = do
    let x = A.encode (1 :: Int)
    print x
    putStrLn "encode int ok"
