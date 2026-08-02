-- Let/where pattern bindings with unparenthesised infix constructor
-- operators (Haskell Report §3.12 — all let/where pattern matches are
-- irrefutable).  Mirrors GHC.Internal.Base's Monad NonEmpty:
--
--   ~(a :| as) >>= f = b :| (bs ++ bs')
--     where b :| bs = f a
--           toList ~(c :| cs) = c : cs
--
-- Pre-fix the parser treated `b :| bs = …` as a function binding named
-- `b`, then failed with "expected `=` or `|`; saw TkSymOp \":|\"", so the
-- where-clause (and thus the whole method) never materialised — leaving
-- Warp's exception path on a Method placeholder / PatternMatchFail loop.

infixr 5 :|
data NE a = a :| [a]
  deriving (Show)

main :: IO ()
main = do
    -- Unparenthesised infix conop pattern in let
    let x = 1 :| [2, 3] :: NE Int
        a :| as = x
    print a
    print as
    -- List cons is the same grammar
    let y:ys = [10, 20, 30 :: Int]
    print y
    print ys
    -- where-clause form used by Monad NonEmpty
    print (splitNE (7 :| [8, 9]))
  where
    splitNE ne = (h, t)
      where
        h :| t = ne
