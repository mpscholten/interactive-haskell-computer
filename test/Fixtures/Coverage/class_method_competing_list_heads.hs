{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

data Mark = Mark

type Letters = [Char]
type Marks = [Mark]

class Classify a where
    unaryListHeadChoiceZ :: a -> Int

instance Classify Letters where
    unaryListHeadChoiceZ _ = 20

instance Classify Marks where
    unaryListHeadChoiceZ _ = 22

main :: Int
main = unaryListHeadChoiceZ "letters"
