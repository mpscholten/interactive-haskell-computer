data Text = Text
data Box a = Box { boxValue :: a, boxCount :: Int }

class Choose a where
    first :: Box a -> Int
    second :: Box a -> Int

instance Choose Text where
    first _ = 43
    second box = first box

main :: Int
main = second (Box Text 9)
