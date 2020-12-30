quicksort :: Ord a => [a] -> [a]
quicksort []        = []
quicksort (p:xs)    = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

quicksort2 :: Ord a => [a] -> [a]
quicksort2 []        = []
quicksort2 (x:xs) = quicksort2 [y | y <- xs, y <= x] ++ [x] ++ quicksort2 [y | y <- xs, y > x]

-- sort with function
a `cmp` b = a < b

-- whatever the right declaration might be...
-- quicksort3 :: Ord a => [a] -> [a]
quicksort3 _ []        = []
quicksort3 fn (x:xs) = quicksort2 [y | y <- xs, fn y x] ++ [x] ++ quicksort2 [y | y <- xs, fn x y]

mergesort_merge :: (Ord a) => [a] -> [a] -> [a]
mergesort_merge [] xs = xs
mergesort_merge xs [] = xs
mergesort_merge (x:xs) (y:ys)
    | (x < y) = x:mergesort_merge xs (y:ys)
    | otherwise = y:mergesort_merge (x:xs) ys

mergesort_splitinhalf :: [a] -> ([a], [a])
mergesort_splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

mergesort :: (Ord a) => [a] -> [a]
mergesort xs
    | (length xs) > 1 = mergesort_merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort_splitinhalf xs