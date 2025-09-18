myMap :: (a -> b) -> [a] -> [b]
myMap f [] =       []
myMap f (x : xs) = f x : myMap f xs
-- >>> myMap (+1) [1, 2, 3]
-- [2,3,4]
--

myFilter :: ( a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x : xs) | p x       = x : myFilter p xs
                    | otherwise = myFilter p xs
-- >>> myFilter (> 0) []
-- []
-- >>> myFilter (> 0) [-4]
-- []
-- >>> myFilter (> 0) [3, -4]
-- [3]
-- >>> myFilter (> 0) [-2, 3, -4]
-- [3]
-- >>> myFilter (> 0) [1, -2, 3, -4]
-- [1,3]
--

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x : xs)
    | p x = (x : ys, zs)
    | otherwise = (ys, x : zs)
    where (ys, zs) = partition p xs
-- >>> partition (> 0) []
-- ([],[])
-- >>> partition (> 0) [-4]
-- ([],[-4])
-- >>> partition (> 0) [3, -4]
-- ([3],[-4])
-- >>> partition (> 0) [-2, 3, -4]
-- ([3],[-2,-4])
-- >>> partition (> 0) [1, -2, 3, -4]
-- ([3],[-2,-4])
-- ([1,3],[-2,-4])
--

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e []       = e -- fは演算子！
myFoldl f e (x : xs) = myFoldl f (f e x) xs
-- >>> myFoldl (-) 10 [1, 2, 3]
-- 4
--


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f e [] = e
myFoldr f e (x : xs) = f x (myFoldr f e xs)
-- >>> myFoldr (-) 10 [1, 2, 3]
-- -8
--

-- [x * 10 | x <- [1, -2, 3, -4]] = [10, -20, 30, -40]
-- [x | x <- [1, -2, 3, -4], x > 0] = [1, 3]
-- [x + y | x <- [10, 20] , y<- [1, 2]] = [11, 12, 21, 22]

qsort [] = []
qsort (x : xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]
-- >>> qsort [3, 4, 2, 5, 1]
-- [1,2,3,4,5]
--

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = split 