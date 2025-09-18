myGcd :: Int -> Int -> Int
myGcd x 0 = x
myGcd x y | y > x     = myGcd y x
          | otherwise = myGcd (x - y) y
-- >>> myGcd 12 32
-- 4
-- >>> myGcd 32 12
-- 4
--

range :: Int -> Int -> [Int]
range m n | n < m     = []
          | otherwise = m : range (m + 1) n
--- >>> range 10 15
--- [10,11,12,13,14,15]
--- >>> range 10 9
--- []
---
        
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys) | x < y     = x : y : ys 
                  | otherwise = y : insert x ys
-- >>> insert 5 [2, 2, 4, 6]
-- [2,2,4,5,6]
--
-- >>> insert 7 [2, 2, 4, 6]
-- [2,2,4,6,7]
--

isort :: [Int] -> [Int]
isort [] = []
isort (x : xs) = insert x (isort xs)
-- >>> isort [5, 2, 3, 2]
-- [2,2,3,5]
--
