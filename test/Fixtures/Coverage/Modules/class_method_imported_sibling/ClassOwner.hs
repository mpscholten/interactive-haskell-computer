module ClassOwner (Box(..), Choose(..)) where

data Box a = Box a

class Choose a where
    first :: Box a -> Int
    second :: Box a -> Int
