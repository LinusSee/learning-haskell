
val = [
        [ True, False, False ],
        [ True, True, False ],
        [ False, False, True ]
    ]


neighbours :: [Bool] -> [Int]
neighbours [] = [0]
neighbours [x] = [0]
neighbours (x:y:xs)
  | x && y    = [1, 1] ++ next_neighbours
  | x         = [0, 1] ++ next_neighbours
  | y         = [1, 0] ++ next_neighbours
  | otherwise = [0, 0] ++ next_neighbours
  where next_neighbours = neighbours (y:xs)
-- Odd cases / first two matches should probably be an empty list


main :: IO ()
--main = putStrLn "Hello, world :)"
main = let mapped = map neighbours val
       in putStrLn . show $ mapped