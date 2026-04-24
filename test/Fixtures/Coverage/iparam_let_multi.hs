-- Multi-binding implicit-parameter let in layout mode, as used by
-- IHP's RouterSupport.withImplicits / Test.Mocking.withContext.
main = let ?a = 10
           ?b = 20
           ?c = 12
       in print (?a + ?b + ?c)
