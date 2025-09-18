module TRS where
    
import Data.List

data Term     = V String | F String [Term] deriving (Eq)
type Subst    = [(String, Term)]
type Rule     = (Term, Term)
type TRS      = [Rule]
type Position = [Int]

instance Show Term where
    show (V x)    = x   
    show (F f ts) = f ++ " (" ++ intercalate ", " [show t | t <- ts] ++ " )"

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [ showRule rule | rule <- trs ]

-- Pos(t)
positions :: Term -> [Position]
positions (V _ )  = [[]] -- ++されるのでよい、そもそもPositionは[Int]だった
positions (F _ t) = [[]] ++ [ i : p | (ti, i) <- zip t [0..], p <- positions ti] -- [Position]を返す、それぞれのtとリスト内容を組み付ける、その個別のPositionsを更に計算する
-- >>> positions (F "add" [F "add" [V "x", V "y"], V "y"])
-- [[],[0],[0,0],[0,1],[1]]
--

-- subtermAt t p = t|_p
subtermAt :: Term -> Position -> Term
subtermAt t []             = t
subtermAt (F _ t) (i : q)  = subtermAt (t !! i) q --  !!でインデックスを指定して取り出す
subtermAt (V _) (_ : _)    = error "変数の子を探そうとしています(が未定義)"
-- >>> subtermAt (F "add" [F "add" [V "x", V "y"], V "y"]) [0, 0]
-- x
--

-- replace t u p = t[u]_p
replaceList :: [a] -> Int -> a -> [a]
replaceList xs i new = take i xs ++ [new] ++ drop (i + 1) xs -- take i xsで、先頭からi個の要素、drop (i + 1) xsで、i + 1番目以降の要素をそれぞれ取り出す
-- >>> replaceList [1, 2, 3, 4, 5] 2 99
-- [1,2,99,4,5]
--
replace :: Term -> Term -> Position -> Term
replace _ u []             = u
replace (F f t) u (i : q)  = F f (replaceList t i (replace (t !! i) u q)) -- Termを返すのでF f Termで、Termの中では再帰する、インデックスをみる
replace (V _) _ (_ : _)    = error "変数の子を探そうとしています(が未定義)"
-- >>> replace (F "add" [F "add" [V "x", V "y"], V "y"]) (F "0" []) [0, 0]
-- add ( add ( 0 (  ) , y ) , y ) 
--
-- F "add" [F "add" [F "0" [], V "y"], V "y"]

--substitute t sigma = t sigma
substitute :: Term -> Subst -> Term
substitute (V t) sigma = case lookup t sigma of -- lookupでsigmaにあるかもしれないtに対応する項を探索
    Just term -> term -- termが見つかった場合、tをtermに書き換える
    Nothing   -> V t  -- termが見つからなかった場合、tはそのまま帰す
substitute (F f t) sigma       = F f [substitute ti sigma | ti <- t] --Termを返すのでF f Termで、Termの中はそれぞれに検証
-- >>> substitute (F "add" [F "add" [V "x", V "y"], V "y"]) [("x", F "0" [])]
-- add ( add ( 0 (  ) , y ) , y ) 
--
-- F "add" [F "add" [F "0" [], V "y"], V "y"]

-- match l t = Just sigma, if l sigma = t for some sigma
-- match l t = Nothing, otherwise
-- はじめ空の代入を入れるリストと、対のリストを2つ持って入れていく
addSubst :: Subst -> (String, Term) -> Maybe Subst -- 代入リストに追加できるならそうする
addSubst subst (x, t) =
    case lookup x subst of -- lookupでsubstにあるかもしれないxに対応する項t'を探索
        Just t' | t' /= t -> Nothing -- case内式の評価の結果、もしすでにxに対応する項t'がすでにあって、かつ、それがtと異なるならば、追加できない
        _                 -> Just ((x, t) : subst) -- そうでなければ、(x, t)をsubstに追加
-- >>> addSubst [("x", V "y")] ("x", F "0" [])
-- Nothing
--
-- >>> addSubst [("x", V "y")] ("y", F "0" [])
-- Just [("y",0 (  ) ),("x",y)]
--
matchPairs :: [(Term, Term)] -> Subst -> Maybe Subst -- 対のリストと代入リストを受け取り、もしマッチするならば、更新された代入リストを返す
matchPairs [] subst                = Just subst -- 空の対はいくらでも追加できる
matchPairs ((V x, t) : pairs) subst = -- 変数と項の対の場合
    case addSubst subst (x, t) of -- (x, t)をsubstに追加できるか試す
        Just subst' -> matchPairs pairs subst' -- できたら、pairs(組まれてたもの)とsubst'(addSubst subst (x, t)の評価結果)を使って残りの対を検証
        Nothing     -> Nothing
matchPairs ((F f t, F g u) : pairs) subst -- 関数項と関数項の対の場合
    | f == g && length t == length u = matchPairs (zip t u ++ pairs) subst -- 関数記号が同じで、引数の数も同じならば、引数同士を対にして、残りの対と合わせて検証
    | otherwise                      = Nothing
matchPairs _ _ = Nothing -- 変数と項の対、項と項の対以外の場合はマッチしない
-- >>> matchPairs [(F "add" [V "x", V "y"], F "add" [V "x", V "y"]), (V "y", V "y")] []
-- Just [("y",y),("y",y),("x",x)]
--
-- >>> matchPairs [(F "add" [V "x", V "y"], F "add" [F "add" [V "x", V "y"], V "y"])] []
-- Just [("y",y),("x",add ( x, y ) )]
--
match :: Term -> Term -> Maybe Subst
match l t = matchPairs [(l, t)] [] -- 対のリスト1つだけと空の代入リスト
-- >>> match (F "add" [F "add" [V "x", V "y"], V "y"]) (F "add" [V "x", V "y"])
-- Nothing
--
-- >>> match (F "add" [V "x", V "y"]) (V "x")
-- Nothing
--
-- >>> match (F "add" [V "x", V "y"]) (F "add" [F "add" [V "x", V "y"], V "y"])
-- Just [("y",y),("x",add ( x, y ) )]
--
-- >>> match (F "add" [F "add" [F "add" [V "w", V "v"], V "x"], V "z"]) (F "add" [F "add" [F "add" [V "c", V "d"], V "b"], V "a"])
-- Just [("z",a),("x",b),("v",d),("w",c)]
--

-- rewrite R t = Just u, if t -> R u for some term u
-- rewrite R t = Nothing, otherwise
reducts :: TRS -> Term -> [Term]
reducts rule t = 
    [ replace t (substitute r sigma) p | -- 置換したrをp位置に置き換える
      p <- positions t, -- tのすべての位置pについて
      (l, r) <- rule,
      Just sigma <- [match l (subtermAt t p)]] -- tのp位置の部分項とlがマッチするならば、その代入sigmaを使う
-- >>> reducts [ (F "add" [F "0" [], V "y"], V "y"), (F "add" [F "s" [V "x"], V "y"], F "s" [F "add" [V "x", V "y"]]) ] (F "add" [F "s" [F "0" []], F "s" [F "s" [F "0" []]]])
-- [s ( add ( 0 (  ) , s ( s ( 0 (  )  )  )  )  ) ]
--
rewrite :: TRS -> Term -> Maybe Term
rewrite rules t = case reducts rules t of
    (u : _) -> Just u  -- もし変換できるならば、最初のものを返す
    []      -> Nothing -- 変換できなかった
-- >>> rewrite [ (F "add" [F "0" [], V "y"], V "y"), (F "add" [F "s" [V "x"], V "y"], F "s" [F "add" [V "x", V "y"]]) ] (F "add" [F "s" [F "0" []], F "s" [F "s" [F "0" []]]])
-- Just s ( add ( 0 (  ) , s ( s ( 0 (  )  )  )  )  ) 
--

-- nf R t = u  if t -> R ... -> _R u for some normal form u.
nf :: TRS -> Term -> Term
nf rules t = case rewrite rules t of -- もしtを変換できるならば
    Just u -> nf rules u -- 変換したuを更に変換する
    Nothing -> t
-- trs = { add(0,y) -> y, add(s(x), y) -> s(add(x,y)) }
-- >>> rule1 = (F "add" [F "0" [], V "y"], V "y")
-- >>> rule2 = (F "add" [F "s" [V "x"], V "y"], F "s" [F "add" [V "x", V "y"]])
-- >>> trs = [rule1, rule2]
--
-- a normal form of add(s(0), s(s(0))):
-- >>> nf [(F "add" [F "0" [], V "y"], V "y"), (F "add" [F "s" [V "x"], V "y"], F "s" [F "add" [V "x", V "y"]])] (F "add" [F "s" [F "0" []], F "s" [F "s" [F "0" []]]])
-- s (s (s (0 ( ) ) ) )
--
-- which is identical to: F "s" [F "s" [F "s" [F "0" []]]]

