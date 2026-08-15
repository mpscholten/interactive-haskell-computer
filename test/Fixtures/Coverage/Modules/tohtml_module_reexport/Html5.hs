module Html5
    ( module Html
    , wrap
    ) where

import Html

wrap :: Html -> Html
wrap (Content s) = Content ("<h1>" ++ s ++ "</h1>")
