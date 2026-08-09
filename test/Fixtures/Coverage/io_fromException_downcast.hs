{-# LANGUAGE DeriveDataTypeable #-}

-- Result-type-directed dispatch for the source-defined Exception selectors.
-- The demanded constructor under Just selects the corresponding dictionary;
-- no host toException/fromException shim participates.
import Data.Typeable (Typeable)
import GHC.Internal.Exception.Type (Exception(..), SomeException(..))

data MyErr = MyErr deriving (Show, Typeable)
data Other = Other deriving (Show, Typeable)

instance Exception MyErr where
    fromException _ = Just MyErr

instance Exception Other where
    fromException _ = Nothing

main :: IO ()
main = do
    let some = SomeException MyErr
    case fromException some of
        Just MyErr -> putStrLn "myerr: matched"
        Nothing -> putStrLn "myerr: nothing"
    case fromException some of
        Just Other -> putStrLn "other: matched"
        Nothing -> putStrLn "other: nothing"
