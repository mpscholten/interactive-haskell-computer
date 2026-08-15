-- OverloadedStrings at expected Markup/Html (blaze H.h1 "Hello world").
-- Equality-constrained IsString (MarkupM a) with a ~ (), plus Html/Markup
-- synonyms.  Structural fromString from the callee/annotation type — no
-- blaze name list, no host shim.  Custom ADT so this is not a package run.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Data.String (IsString(..))

data MarkupM a
    = Parent String String String (MarkupM a)
    | Content String a
    | Empty a
    deriving (Show)

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
    putStrLn (render (h1 "Hello world"))
    putStrLn (render ("Hello world" :: Html))
    putStrLn (render ("Hello world" :: Markup))
    putStrLn (render ("Hello world" :: MarkupM ()))
    putStrLn (render (fromString "Hello world" :: Html))
