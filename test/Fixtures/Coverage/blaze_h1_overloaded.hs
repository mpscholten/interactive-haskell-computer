-- Blaze H.h1 "Hello world" via OverloadedStrings.  IsString (MarkupM a)
-- at Html, inner fromString = String (ChoiceString).  No toHtml / ToMarkup.
-- Reduced sibling: istring_choice_ctor_string.hs.
{-# LANGUAGE OverloadedStrings #-}
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = putStrLn (renderHtml (H.h1 "Hello world"))
