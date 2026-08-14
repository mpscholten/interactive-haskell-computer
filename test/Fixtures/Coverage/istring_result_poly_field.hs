-- Result-polymorphic IsString.fromString must take the instance from
-- the expected type (annotation or constructor field), not from a
-- leftover last-writer dispatcher.  Forcing another IsString instance
-- first models Settings/Text loading before a later HostPreference
-- field / annotation.  Custom ADT so this is not a name list of
-- HostPreference / "*4".
{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(..))
import qualified Data.ByteString.Char8 as C8

data Pref = Any | V4 | Named String
    deriving (Eq, Show)

instance IsString Pref where
    fromString "*"  = Any
    fromString "*4" = V4
    fromString s    = Named s

data Rec = Rec { recHost :: Pref, recPort :: Int }
    deriving (Eq, Show)

defaultRec :: Rec
defaultRec = Rec { recHost = "*4", recPort = 3000 }

main :: IO ()
main = do
    print (C8.pack "x" == ("x" :: C8.ByteString))
    print (fromString "*4" :: Pref)
    print (recHost defaultRec)
    print (recHost (defaultRec { recPort = 13099 }))
