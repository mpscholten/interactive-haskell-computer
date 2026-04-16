-- ExistentialQuantification: `data Box = forall a. Show a => Box a`
-- Each Box wraps a value of some type together with its Show dict.
-- Parser must accept the `forall a.` + constraint context before the ctor name.
data Box = forall a. Show a => Box a

unBox :: Box -> String
unBox (Box x) = show x

main = do
    putStrLn (unBox (Box (42 :: Int)))
    putStrLn (unBox (Box "hello"))
    putStrLn (unBox (Box True))
