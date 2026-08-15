module Html
    ( module Markup
    , Html
    , toHtml
    ) where

import Markup

type Html = Markup

toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup
