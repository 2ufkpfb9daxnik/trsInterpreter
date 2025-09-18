data Tree = Leaf | Node Tree Int Tree
tree1 :: Tree
tree1 = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]
-- >>> postorder tree1
-- [1,3,5,4,2]
--

have :: Eq a => a -> [a] -> Bool
have _ [] = False
have x (y : ys)
    | x == y    = True
    | otherwise = have x ys

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs)
    | have x xs = nub xs
    | otherwise = x : nub xs

-- >>> nub [1, 2, 3, 3, 3, 2, 4, 1]
-- [3,2,4,1]
--
-- >>> nub "apple"
-- "aple"
--