-- data Tree = Leaf | Node Tree Int Tree -- Tree型の定義、Leafは空(終点)、Nodeは左のTree、値、右のTreeを持つという定義(二分木)

tree1 :: Tree
tree1 = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

member :: Int -> Tree -> Bool
member x Leaf = False
member x (Node l y r) =
    x == y || member x l || member x r
-- >>> member 3 tree1
-- True
--

inorder :: Tree -> [Int]
inorder Leaf         = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r
-- >>> inorder tree1
-- [1,2,3,4,5]
--

preorder :: Tree -> [Int]
preorder Leaf = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r
-- >>> preorder tree1
-- [2,1,4,3,5]
--

postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]
-- >>> postorder tree1
-- [1,3,5,4,2]
--


class Size a where
    size :: a -> Int

instance Size [a] where
    size xs = length xs

instance Size Tree where
    size Leaf = 0
    size (Node l x r) = 1 + size l + size r

totalSize :: Size a => [a] -> Int
totalSize xs = sum [ size x | x <- xs]

-- >>> totalSize [[1,2,3], [4,5], []]
-- 5
-- >>> totalSize [tree1, Node Leaf 10 Leaf]
-- 6
--


-- instance Show Tree where -- Haskellの標準型クラス Show のインスタンスをTree型に対して定義
--     show Leaf = "Leaf"
--     show (Node l x r) =
--         "(Node" ++ show l ++ " " ++ show x ++ " " ++ show r ++ ")"


-- instance Eq Tree where -- Haskellの標準型クラス Eq のインスタンスをTree型に対して定義
--     Leaf          == Leaf          = True
--     Node l1 x1 r1 == Node l2 x2 r2 = x1 == x2 && l1 == l2 && r1 == r2


-- data Tree = Leaf | Node Tree Int Tree
--     deriving Show -- Show型クラスのインスタンスを自動生成

-- data Tree = Leaf | Node Tree Int Tree
--     deriving Eq

data Tree = Leaf | Node Tree Int Tree
    deriving (Show, Eq) -- Show型クラスとEq型クラスのインスタンスを自動生成


type Forest = [Tree]
-- data [a] = [] | a : [a] -- リストの定義、[]は空リスト、:はリストの先頭に値を追加する
-- data Bool = False | True -- Bool型の定義、FalseとTrueの2つの値を持つ
-- data Maybe a = Nothing | Just a -- Maybe型の定義、Nothingは値がないことを表し、Just aは値aがあることを表す
-- type String = [Char]


-- for a = [(x1, y1), (x2, y2), ..., (xn, yn)]
-- myLookup x a = Just yk if x == xi for some i
--                Nothing otherwise
-- where k = min{i | x == xi}
myLookup :: Eq k => k -> [(k, v)] -> Maybe v
myLookup _ [] = Nothing -- _は使わないことを表すワイルドカード
myLookup x ((k, v): as)
    | x == k    = Just v
    | otherwise = myLookup x as
-- >>> myLookup 2 [(1, "Jan"), (2, "Feb")]
-- Just "Feb"
-- >>> myLookup 3 [(1, "Jan"), (2, "Feb")]
-- Nothing
--


monthNames = [(1, "Jan"), (2, "Feb"), (3, "Mar"), (4, "Apr"), (5, "May"), (6, "Jun"), (7, "Jul"), (8, "Aug"), (9, "Sep"), (10, "Oct"), (11, "Nov"), (12, "Dec")]
monthName1 n =
    case lookup n monthNames of
        Just s  -> s
        Nothing -> "not found"
-- >>> monthName1 3
-- "Mar"
--
monthName2 n
    | Just s <- lookup n monthNames = s
    | otherwise                     = "not found"
-- >>> monthName2 13
-- "not found"
--


partition p [] = ([], []) -- pは条件
partition p (x : xs)
    | p x       = (x : ys, zs) -- pは条件で、xが真(満たす)なら
    | otherwise = (ys, x : zs)
    where (ys, zs) = partition p xs
-- >>> partition even [1,2,3,4]
-- ([2,4],[1,3])
--
partition p [] = ([], [])
partition p (x : xs)
    | True <- p x, (ys, zs) <- partition p xs = -- p xが真で、かつ、partition p xsが(ys, zs)に分解できるなら
        (x : ys, zs)
    | (ys, zs) <- partition p xs = -- そうではなくて(p xが偽で)、かつ、partition p xsが(ys, zs)に分解できるなら
        (ys, x : zs)

-- >>> partition even [1,2,3,4]
-- ([2,4],[1,3])
-- 

test1 = [x + y | (x, y) <- [(1, 10), (2, 20)]] -- [11,22]
test2 = [x | Just x <- [Just 1, Nothing, Just 2]] -- [1, 2]
