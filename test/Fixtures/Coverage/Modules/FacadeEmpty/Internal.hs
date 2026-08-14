-- Ordinary value binding that happens to share a class-method name
-- (Alternative.empty).  Not a class method.
module Modules.FacadeEmpty.Internal (TipBin(..), empty, isEmpty) where

data TipBin a = Tip | Bin a

empty :: TipBin a
empty = Tip

isEmpty :: TipBin a -> Bool
isEmpty Tip = True
isEmpty (Bin _) = False
