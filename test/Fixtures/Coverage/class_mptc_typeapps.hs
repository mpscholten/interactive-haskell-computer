-- Multi-parameter class dispatch using TypeApplications
--
-- Exercises the composite-key lookup in the ClassRegistry: the
-- instance head's two types are stored as a [TypeTag] list and
-- at the call site the accumulated @... @... type-args select
-- which instance fires.
--
-- Without multi-key dispatch every call would go to the same
-- (first-registered) instance — here both instances share the
-- class name @MkLabel@ but differ in their Symbol parameter, so
-- only multi-key dispatch can distinguish them.

data Tag = Tag

class MkLabel field value where
    mkLabel :: value -> [Char]

instance MkLabel "name" Int where
    mkLabel v = "name=" ++ show v

instance MkLabel "age" Int where
    mkLabel v = "age=" ++ show v

instance MkLabel "name" Tag where
    mkLabel _ = "name[tag]"

instance MkLabel "age" Tag where
    mkLabel _ = "age[tag]"

main = do
    putStrLn (mkLabel @"name" @Int 42)
    putStrLn (mkLabel @"age"  @Int 30)
    putStrLn (mkLabel @"name" @Tag Tag)
    putStrLn (mkLabel @"age"  @Tag Tag)
