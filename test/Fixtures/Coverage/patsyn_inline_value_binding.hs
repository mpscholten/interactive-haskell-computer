{-# LANGUAGE PatternSynonyms #-}
-- Regression: a simple inline pattern synonym @pattern Name = body@
-- must register as a value binding, so the body can be used in
-- expression position.  Network.Socket.Options uses this form for ~33
-- SocketOption constants (NoDelay, KeepAlive, ReuseAddr, etc.); the
-- host-side per-name shims previously masked this path.
--
-- Existing patsyn fixtures only cover unidirectional (@<-@) and
-- explicit-where (@<- ... where@) forms.  This pins down the inline
-- @=@ form, which is handled by the binding scanner's
-- @handleTopPattern@ (src/IHC/Scan.hs) registering @Bar@ as if it were
-- an ordinary @Bar = Foo 6 1@ value binding.

data Foo = Foo Int Int deriving Show

pattern Bar :: Foo
pattern Bar = Foo 6 1

main :: IO ()
main = do
    case Bar of                       -- expression position
        Foo a b -> putStrLn ("expr " ++ show a ++ " " ++ show b)
    case Bar of                       -- pattern position (tryPatSyn)
        Bar -> putStrLn "patmatch ok"
        _   -> putStrLn "patmatch fail"
