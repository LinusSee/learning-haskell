fibonacci   :: Integer -> Integer
fibonacci 0 =  0
fibonacci 1 =  1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


myMaximum        :: (Ord a) => [ a ] -> a
myMaximum []     = error "Empty list can't have a maximum"
myMaximum [ x ]  =  x
myMaximum (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = myMaximum xs


replicate'     :: (Num i, Ord i) => i -> i -> [i]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ []     = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x:take' (n - 1) xs


reverse'        :: (Ord i) => [i] -> [i]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat'   :: i -> [i]
repeat' x = x : repeat' x


zip'               :: [a] -> [b] -> [(a, b)]
zip' [] _          =  []
zip' _ []          =  []
zip' (x:xs) (y:ys) = (x, y) : (zip xs ys)


elem'           :: (Eq a) => a -> [a] -> Bool
elem' y []      =  False
elem' y (x:xs)
    | y == x    =  True
    | otherwise =  y `elem'` xs


quicksort        :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a <= x ]
        greaterSorted = quicksort [ a | a <- xs, a > x ]
    in  smallerSorted ++ [x] ++ greaterSorted


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys
