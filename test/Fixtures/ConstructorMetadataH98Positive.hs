module ConstructorMetadataH98Positive where

data Many a = Empty | Strict !a | Unpacked {-# UNPACK #-} !Int | Wrapped (Maybe a) | Listed [a] | Qualified Data.Int.Int
newtype Single a = Single (Maybe a)
data Records a = Record { firstField, secondField :: !Maybe a, thirdField :: {-# UNPACK #-} !Int } | NoFields
data PosState s = PosState
    { pstateInput :: s
    , pstateOffset :: !Int
    , pstateSourcePos, pstateTabWidth :: Maybe (Either String Int)
    , pstateLinePrefix :: [Char]
    }
