-- Class default method that calls another method of the same class.
-- Instance supplies only the "short" method; default `longDesc`
-- composes on top. Exercises class-method self-dispatch in defaults.
class Describable a where
    shortDesc :: a -> String
    longDesc :: a -> String
    longDesc x = "Description: " ++ shortDesc x

data Vehicle = Car | Truck | Bike

instance Describable Vehicle where
    shortDesc Car   = "car"
    shortDesc Truck = "truck"
    shortDesc Bike  = "bike"

main :: IO ()
main = do
    putStrLn (shortDesc Car)
    putStrLn (longDesc Truck)
    putStrLn (longDesc Bike)
