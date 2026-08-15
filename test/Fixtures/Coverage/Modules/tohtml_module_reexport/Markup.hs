{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Markup (ToMarkup(..), Markup(..), string, render) where

data Markup = Content String
  deriving Show

class ToMarkup a where
    toMarkup :: a -> Markup

instance ToMarkup Markup where
    toMarkup x = x

instance ToMarkup String where
    toMarkup = string

string :: String -> Markup
string s = Content s

render :: Markup -> String
render (Content s) = s
