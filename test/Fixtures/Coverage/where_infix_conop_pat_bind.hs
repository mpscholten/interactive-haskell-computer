-- where-bound unparenthesised `:|` pattern, as in
-- GHC.Internal.Base instance Monad NonEmpty.
infixr 5 :|
data NE a = a :| [a]
  deriving (Show)

bindNE :: NE a -> (a -> NE b) -> NE b
bindNE ~(a :| as) f = b :| (bs ++ bs')
  where
    b :| bs = f a
    bs' = as >>= toList . f
    toList ~(c :| cs) = c : cs

main :: IO ()
main = print (bindNE (1 :| [2]) (\n -> n :| [n + 10]))
