-- Demonstrate user-defined type class instances for Show and Eq.
-- ihc supports instance declarations for the built-in Show and Eq classes.
data Color = Red | Green | Blue | Yellow

instance Show Color where
    show col = case col of
        Red    -> "Red"
        Green  -> "Green"
        Blue   -> "Blue"
        Yellow -> "Yellow"

instance Eq Color where
    (==) Red    Red    = True
    (==) Green  Green  = True
    (==) Blue   Blue   = True
    (==) Yellow Yellow = True
    (==) _      _      = False

isSimilar :: Color -> Color -> String
isSimilar x y =
    if x == y
        then show x ++ " matches " ++ show y
        else show x ++ " differs from " ++ show y

main :: IO ()
main = do
    let colors = [Red, Green, Blue, Yellow]
    printColors colors
    putStrLn (isSimilar Red Red)
    putStrLn (isSimilar Red Blue)
    putStrLn (isSimilar Green Green)

printColors :: [Color] -> IO ()
printColors [] = pure ()
printColors (c : rest) = do
    putStrLn ("Color: " ++ show c)
    printColors rest
