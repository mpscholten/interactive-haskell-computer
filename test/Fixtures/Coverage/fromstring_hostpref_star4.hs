-- fromString "*4" :: HostPreference must pin IsString from the
-- expected type (result-poly), not leftover last-writer dispatcher.
-- Source instance prints HostIPv4.  No interpreter name list.
{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(..))
import Data.Streaming.Network (HostPreference)

main :: IO ()
main = do
    print (fromString "*4" :: HostPreference)
    print ("*4" :: HostPreference)
