-- OverloadedStrings at a function parameter (not an annotation or
-- constructor field) must take IsString from the callee scheme.
-- Desugared "…" is an EApp of (:); that must still elaborate.
-- Unique names so this is not a HostPreference / wrap name list.
{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(..))

data Pref = Named String
    deriving Show

instance IsString Pref where
    fromString = Named

paintPref :: Pref -> String
paintPref (Named s) = "<h1>" ++ s ++ "</h1>"

main :: IO ()
main = putStrLn (paintPref "Hello world")
