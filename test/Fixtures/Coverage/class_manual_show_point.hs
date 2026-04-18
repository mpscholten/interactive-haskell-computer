-- Manual Show instance whose body references `show` on imported Int
-- values. Regression for instance-method bodies losing access to
-- their free imports (the "placeholder" bug pattern).
data Point = Point Int Int

instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

main :: IO ()
main = do
    print (Point 3 4)
    putStrLn (show (Point (-1) 7))
