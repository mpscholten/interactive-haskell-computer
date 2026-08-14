-- Case-alternative `where !x = e` must parse.  Data.Set.insert is
--   go orig !x t@(Bin sz y l r) = case compare x y of
--       GT | otherwise -> balanceR y l r'
--          where !r' = go orig x r
-- parseOneAltWhereBind used to ignore TkBang and retry the same
-- cursor, hanging discovery.  Custom ADT so the fixture does not
-- depend on containers.
data T a = Tip | Bin Int a (T a) (T a)
  deriving (Show)

singleton x = Bin 1 x Tip Tip

insert x0 = go x0 x0
  where
    go orig _ Tip = singleton orig
    go orig x (Bin sz y l r) = case compare x y of
        LT -> Bin sz y l' r
          where !l' = go orig x l
        GT -> Bin sz y l r'
          where !r' = go orig x r
        EQ -> Bin sz orig l r

main :: IO ()
main = print (insert (1 :: Int) (singleton 0))
