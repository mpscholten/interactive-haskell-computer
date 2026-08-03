-- mapM_ writeArray in runSTArray (return () base of mapM_ must not
-- drop ST left actions when it defaults to IO).
{-# LANGUAGE OverloadedStrings #-}
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let xs = [1] :: [Int]
        rsp = runSTArray $ do
            arr <- newArray (0, 3) (Nothing :: Maybe ByteString)
            mapM_ (\i -> writeArray arr i (Just ("ihc" :: ByteString))) xs
            return arr
    case rsp ! 1 of
        Just v  -> putStrLn (C8.unpack v)
        Nothing -> putStrLn "Nothing"
