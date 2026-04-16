-- Test that qualified constructor patterns are parsed correctly.
-- The qualifier prefix is stripped; the underlying constructor name is used.
module Main where

data Color = Red | Green | Blue

-- Qualify with module name in pattern position (stripped to bare 'Red')
main :: IO ()
main = do
    let describe c = case c of
            Main.Red   -> "red"
            Main.Green -> "green"
            Main.Blue  -> "blue"
    putStrLn (describe Red)
    putStrLn (describe Green)
