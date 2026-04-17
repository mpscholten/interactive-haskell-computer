{-# LANGUAGE NamedFieldPuns #-}

data Triple = Triple { x :: Int, y :: Int, z :: Int }

sumXY :: Triple -> Int
sumXY Triple{ x, y } = x + y

main = print (sumXY (Triple { x = 1, y = 2, z = 99 }))
