-- Nested IsString: `fromString x = Content (fromString x) ()` at
-- `instance (a ~ ()) => IsString (MarkupM a)`, plus `fromString = String`
-- where String is a data constructor sharing the Prelude type name.
-- Expected MarkupM () must inhabit the polymorphic instance head
-- (structural pattern, not a Markup/Html name list).  Inner fromString
-- pins to ChoiceString from the Content field type.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Data.String (IsString(..))

data ChoiceString
    = Static String
    | String String
    | Text String
    | PreEscaped ChoiceString
    | EmptyChoiceString

instance IsString ChoiceString where
    fromString = String

data MarkupM a
    = Parent String String String (MarkupM a)
    | Content ChoiceString a
    | Empty a

type Html = MarkupM ()

instance (a ~ ()) => IsString (MarkupM a) where
    fromString x = Content (fromString x) ()

h1 :: Html -> Html
h1 = Parent "h1" "<h1" "</h1>"

fromChoice (Static s) = s
fromChoice (String s) = s
fromChoice (Text s) = s
fromChoice (PreEscaped x) = fromChoice x
fromChoice EmptyChoiceString = ""

render (Parent _ o c i) = o ++ ">" ++ render i ++ c
render (Content cs _) = fromChoice cs
render (Empty _) = ""

main :: IO ()
main = putStrLn (render (h1 "Hello world"))
