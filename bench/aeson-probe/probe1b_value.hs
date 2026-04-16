import qualified Data.Aeson as A

main :: IO ()
main = do
    let v = A.Number 42
    print v
    putStrLn "value ok"
