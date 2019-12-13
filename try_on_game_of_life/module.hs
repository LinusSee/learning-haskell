
val = [
        [ True, False, False ],
        [ True, True, False ],
        [ False, False, True ]
    ]


neighbours :: [Bool] -> [Int]
neighbours [] = []
neighbours [x] = []
neighbours (x:y:xs)
  | x && y    = [1, 1] ++ next_neighbours
  | x         = [0, 1] ++ next_neighbours
  | y         = [1, 0] ++ next_neighbours
  | otherwise = [0, 0] ++ next_neighbours
  where next_neighbours = neighbours (y:xs)
-- Odd cases / first two matches should probably be an empty list

merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge [x, y] = [x, y]
merge [w, x, y, z] = [w, x + y, z]
merge (x:y:w:xs) = [start, acc] ++ merge (w:xs)
  where start = x
        acc = y + w

main :: IO ()
--main = putStrLn "Hello, world :)"
main = let mapped = map (merge . neighbours) val
       in putStrLn . show $ mapped
