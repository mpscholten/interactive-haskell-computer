-- Blaze-shaped IsString: `instance (a ~ ()) => IsString (MarkupM a)`
-- registered as `MarkupM a`, while expected `Html` / `Markup` /
-- `MarkupM ()` pins `MarkupM ()`.  Structural pattern match, not a
-- ToMarkup / toHtml / Html name list.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Data.String (IsString(..))

data MarkupM a
    = Parent String String String (MarkupM a)
    | Content String a
    | Empty a

type Markup = MarkupM ()
type Html = Markup

instance (a ~ ()) => IsString (MarkupM a) where
    fromString x = Content x ()

h1 :: Html -> Html
h1 = Parent "h1" "<h1" "</h1>"

render :: MarkupM a -> String
render (Parent _ open close inner) = open ++ ">" ++ render inner ++ close
render (Content s _) = s
render (Empty _) = ""

main :: IO ()
main = do
    putStrLn (render ("Hello world" :: MarkupM ()))
    putStrLn (render ("Hello world" :: Markup))
    putStrLn (render ("Hello world" :: Html))
    putStrLn (render (h1 "Hello world"))
