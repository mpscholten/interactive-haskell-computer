import qualified Html5 as H

main :: IO ()
main = putStrLn (render (H.toHtml ("Hello world" :: String)))
