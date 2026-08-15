-- H.h1 "Hello world" shape without blaze names: function argument of
-- a two-hop synonym for an applied carrier whose IsString instance
-- is `(a ~ ()) => IsString (Carrier a)`.  Dispatch is by the expanded
-- structural tag (`Carrier ()`), not a Markup / Html name list.
{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(..))

data Carrier a
    = Nest (Carrier a)
    | Payload String a

type Markup = Carrier ()
type Html = Markup

instance (a ~ ()) => IsString (Carrier a) where
    fromString s = Payload s ()

paintHtml :: Html -> Html
paintHtml = Nest

showDoc :: Carrier a -> String
showDoc (Nest inner) = "<h1>" ++ showDoc inner ++ "</h1>"
showDoc (Payload s _) = s

main :: IO ()
main = putStrLn (showDoc (paintHtml "Hello world"))
