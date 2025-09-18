myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y): xys) = (x : xs, y : ys)
    where (xs, ys) = myUnzip xys
-- >>> myUnzip [(1, 2), (3, 4), (5, 6), (7, 8)]
-- ([1,3,5,7],[2,4,6,8])
--

power :: [a] -> [[a]]
power [] = [[]]
power (x : xs) = ys ++ map ( x : ) ys
    where ys = power xs
-- >>> power [3]
-- [[],[3]]
-- >>> power [2, 3]
-- [[],[3],[2],[2,3]]
-- >>> power [1, 2, 3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
--

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : [y : zs | zs <- interleave x ys]
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = [ ys | ps <- permutations xs, ys <- interleave x ps ]
-- [1, 2, 3, 4] !! 2 = 3 -- !!はリストのそのインデックスの値を取り出す
safe :: [Int] -> Bool
safe [] = False
safe xs =
    let sums = [ xs !! 0 + xs !! 1 + xs !! 2,
                 xs !! 3 + xs !! 4 + xs !! 5,
                 xs !! 6 + xs !! 7 + xs !! 8,
                 xs !! 0 + xs !! 3 + xs !! 6,
                 xs !! 1 + xs !! 4 + xs !! 7,
                 xs !! 2 + xs !! 5 + xs !! 8,
                 xs !! 0 + xs !! 4 + xs !! 8,
                 xs !! 2 + xs !! 4 + xs !! 6]
    in all (== head sums) (tail sums) -- all (==15) [15, 15, 15] : True, all (==15) [15, 14, 15] : False
magicSquare :: [[Int]]
magicSquare = [xs | xs <- permutations [1..9], safe xs]
-- >>> magicSquare
-- [[6,1,8,7,5,3,2,9,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,9,2,3,5,7,8,1,6]]
--

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show
tree1 :: Tree Int
tree1 = Node (Node (Node Leaf 0 Leaf) 1 (Node (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)) 5 Leaf)) 6 (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf))

deleteMax :: Tree a -> (a, Tree a)
deleteMax (Node left y Leaf) = (y, left) -- (maxLeft, newLeft)
deleteMax (Node left y right) =
    let (maxVal, newRight) = deleteMax right
    in (maxVal, Node left y newRight)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node left y right)
    | x < y     = Node (delete x left) y right
    | x > y     = Node left y (delete x right)
    | otherwise = 
        case (left, right) of
            (Leaf, _) -> right
            (_, Leaf) -> left
            _         -> 
                let (maxLeft, newLeft) = deleteMax left
                in Node newLeft maxLeft right
-- >>> delete 5 tree1
-- Node (Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf))) 6 (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf))
--
-- >>> delete 6 tree1
-- Node (Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf))) 5 (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf))
--

