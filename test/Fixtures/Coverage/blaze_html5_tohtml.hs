-- Leftover after HSX TH/Blaze sink: H.toHtml through
-- `module Text.Blaze.Html` re-export on Text.Blaze.Html5.
-- `import Text.Blaze.Html (toHtml)` was already GREEN; the facade
-- hop must resolve to the defining alias, not an unapplied dispatcher.
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = putStrLn (renderHtml (H.toHtml ("Hello world" :: String)))
