-- examples/blaze_hello shape: Html5.h1 of Html5.toHtml String,
-- then renderHtml.  Exercises the facade re-export of toHtml
-- as an argument to a local Html5 combinator.
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = putStrLn (renderHtml (H.h1 (H.toHtml ("Hello world" :: String))))
