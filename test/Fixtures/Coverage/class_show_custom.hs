-- Custom Show instance with custom rendering
data Shape = Circle Int | Rectangle Int Int | Triangle Int Int Int

instance Show Shape where
    show (Circle r)        = "Circle(r=" ++ show r ++ ")"
    show (Rectangle w h)   = "Rect(" ++ show w ++ "x" ++ show h ++ ")"
    show (Triangle a b c)  = "Tri(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

area (Circle r)       = r * r
area (Rectangle w h)  = w * h
area (Triangle _ b h) = b * h `div` 2

printEach [] = pure ()
printEach (x:xs) = do
    putStrLn (show x)
    print (area x)
    printEach xs

main = printEach [Circle 5, Rectangle 3 4, Triangle 3 4 5]
