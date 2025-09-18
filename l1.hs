squareSum :: Int -> Int -> Int
squareSum x y = x^2 + y^2
--- >>> squareSum 3 4
--- 25
---

identity :: a -> a
identity x = x
--- >>> identity 5
--- 5
---

sum1 :: Int -> Int
sum1 n =
    if n == 0
        then 
            0
        else
            n + sum1 (n - 1)

--- >>> sum1 5
--- 15
---

sum2 :: Int -> Int
sum2 n | n == 0    = 0
       | otherwise = n + sum2 (n - 1)

sum3 :: Int -> Int
sum3 0 = 0
sum3 n = n + sum3 (n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n - 1) * n

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) =  1 + myLength xs -- xはリストの先頭、xsはリストの残り

-- リストは空リストに後ろから値を付け足していく
--- >>> myLength (1:(2:(3:[])))
--- 3
---
--- >>> myLength (1:2:3:[])
--- 3
---
--- >>> myLength [1, 2, 3]
--- 3
---

append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : append xs ys -- 右結合なので、:のあとに返されるのはリストなので()はいらない
--- >>> append (1 : 2 : 3 : []) (4 : 5 : [])
--- [1,2,3,4,5]
---

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs
--- >>> sumList [1, 2, 3, 4]
--- 10
---

evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens (x : y : xs) = x : evens xs
--- >>> evens [1, 2, 3, 4, 5, 6]
--- [1,3,5]
--- >>> evens [1, 2, 3, 4, 5]
--- [1,3,5]
---


