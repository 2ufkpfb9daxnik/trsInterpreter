sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs
-- >>> sumList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- 45
--

oddplus1_old :: [Int] -> [Int]
oddplus1_old [] = []
oddplus1_old xs = map (+1) (filter (\x -> mod x 2 == 1) xs)
-- >>> oddplus1_old [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- [2,4,6,8,10]
--

oddplus1 :: [Int] -> [Int]
oddplus1 [] = [] 
oddplus1 xs = [x + 1 | x <- xs, mod x 2 == 1]
-- >>> oddplus1 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- [2,4,6,8,10]
--

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) | x < y     = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys
-- >>> merge [1, 3, 5] [2, 4, 6]
-- [1,2,3,4,5,6]
--

split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : zs) = (x : xs, y : ys)
    where (xs, ys) = split zs
-- >>> split [0, 1, 2, 3, 4, 5]
-- ([0,2,4],[1,3,5])
--

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = split xs
-- >>> msort [3, 4, 2, 5, 1]
-- [1,2,3,4,5]
--
