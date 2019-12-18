import Data.List

val = [
        [ True, False, False ],
        [ True, True, False ],
        [ False, False, True ]
    ]

val_alive = [
        (0, 0), (1, 0), (1, 1), (2, 2)
    ]

type Point = (Integer, Integer)

all_points :: [Point] -> [Point]
all_points [] = []
all_points (x:xs) = nub $ neighbours x ++ all_points xs

neighbours :: Point -> [Point]
neighbours (x, y) = combs xs ys (x, y)
  where xs = [x - 1, x, x + 1]
        ys = [y - 1, y, y + 1]

combs :: [Integer] -> [Integer] -> Point -> [Point]
combs [] _ _ = []
combs _ [] _ = []
combs (x:xs) (y:ys) p = [(x, y)] ++ combs (x:xs) ys p ++ combs xs (y:ys) p



main :: IO ()
--main = putStrLn "Hello, world :)"
main = putStrLn $ show $ all_points val_alive
