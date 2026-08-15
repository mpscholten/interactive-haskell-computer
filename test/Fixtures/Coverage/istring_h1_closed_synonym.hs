-- OverloadedStrings at a type synonym of an applied constructor.
-- `h1 "Hello world"` must use the callee scheme `Html -> Html`
-- (`Html = Markup = MarkupM ()`), not the residual `MarkupM a`
-- from `h1 = Parent …`.  Closed instance head, no `a ~ ()`.
{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(..))

data MarkupM a
    = Parent String String String (MarkupM a)
    | Content String a
    | Empty a

type Markup = MarkupM ()
type Html = Markup

instance IsString (MarkupM ()) where
    fromString x = Content x ()

h1 :: Html -> Html
h1 = Parent "h1" "<h1" "</h1>"

render :: MarkupM a -> String
render (Parent _ open close inner) = open ++ ">" ++ render inner ++ close
render (Content s _) = s
render (Empty _) = ""

main :: IO ()
main = do
    putStrLn (render ("Hello world" :: Html))
    putStrLn (render (h1 "Hello world"))
