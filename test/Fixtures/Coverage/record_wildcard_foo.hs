-- RecordWildCards leftover: `Foo {..}` in construction and pattern.
-- Construction reads same-named vars from scope; the pattern binds
-- them back.  Field force after wildcard must not leave leftover
-- selectors.
{-# LANGUAGE RecordWildCards #-}

data Foo = Foo { fooX :: Int, fooY :: Int }

mkFoo :: Int -> Int -> Foo
mkFoo fooX fooY = Foo {..}

sumFoo :: Foo -> Int
sumFoo Foo{..} = fooX + fooY

main :: IO ()
main = do
    let f = mkFoo 3 4
    print (sumFoo f)
    print (fooX f)
    print (fooY f)
    let fooX = 10
        fooY = 20
        g = Foo {..}
    print (sumFoo g)
