{-# LANGUAGE DeriveGeneric #-}
import qualified Data.Aeson as A
import GHC.Generics (Generic)

data Person = Person { personName :: String, personAge :: Int }
    deriving (Generic, Show)

instance A.ToJSON Person
instance A.FromJSON Person

main :: IO ()
main = do
    let p = Person { personName = "Alice", personAge = 30 }
    print p
    let j = A.encode p
    print j
    putStrLn "record encode ok"
