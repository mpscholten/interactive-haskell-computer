module InstanceOwner (Text(..)) where

import ClassOwner

data Text = Text

instance Choose Text where
    first _ = 51
    second box = first box
