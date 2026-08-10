data Text = Text
data Box a = Box { boxValue :: a, boxCount :: Int } | Count Int

class Choose a where
    first :: Int -> Box a -> Int
    first n box = second n box

    second :: Int -> Box a -> Int
    second n box = first n box

instance Choose Text where
    first _ _ = 41
    second _ _ = 42

main :: Int
main = first 0 (Box Text 9)
