module Main where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.String as R

main :: IO ()
main = putStrLn (R.renderHtml (H.h1 (H.toHtml ("Hello world" :: String))))
