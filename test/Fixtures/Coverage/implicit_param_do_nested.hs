-- Implicit param flows into a do-block / nested let inside the callee.
-- HSX's `parser` is a do-block that reads ?settings / ?extensions.
{-# LANGUAGE ImplicitParams #-}

g :: (?x :: Int) => IO Int
g = do
    let y = ?x
    return y

main :: IO ()
main = do
    n <- let ?x = (99 :: Int) in g
    print n
