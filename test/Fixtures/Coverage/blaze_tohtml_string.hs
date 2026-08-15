-- Direct ToMarkup String as an argument to renderHtml must not
-- be rewritten to `ToMarkup Markup` (`id`).
import Text.Blaze (toMarkup)
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = putStrLn (renderHtml (toMarkup ("Hello world" :: String)))
