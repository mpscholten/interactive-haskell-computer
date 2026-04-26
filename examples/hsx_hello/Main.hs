module Main where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html.Renderer.String as R

main :: IO ()
main = putStrLn (R.renderHtml [hsx|<h1>Hello world</h1>|])
