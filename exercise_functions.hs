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
