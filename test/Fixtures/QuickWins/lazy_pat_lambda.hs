-- Lazy/irrefutable pattern in a lambda parameter.  Mirrors usages in
-- transformers-0.6.1.0/Control/Monad/Trans/State/Lazy.hs, e.g.
--   fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s
main = print ((\ ~(a, b) -> a + b) (10 :: Int, 20 :: Int))
