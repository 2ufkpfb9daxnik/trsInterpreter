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
positions :: Term -> [Position]
positions (V _ )  = [[]] 
positions (F _ t) = [[]] ++ [ i : p | (ti, i) <- zip t [0..], p <- positions ti]
subtermAt :: Term -> Position -> Term
subtermAt t []             = t
subtermAt (F _ t) (i : q)  = subtermAt (t !! i) q
subtermAt (V _) (_ : _)    = error "変数の子を探そうとしています(が未定義)"
replaceList :: [a] -> Int -> a -> [a]
replaceList xs i new = take i xs ++ [new] ++ drop (i + 1) xs
replace :: Term -> Term -> Position -> Term
replace _ u []             = u
replace (F f t) u (i : q)  = F f (replaceList t i (replace (t !! i) u q))
replace (V _) _ (_ : _)    = error "変数の子を探そうとしています(が未定義)"
substitute :: Term -> Subst -> Term
substitute (V t) sigma = case lookup t sigma of 
    Just term -> term
    Nothing   -> V t 
substitute (F f t) sigma       = F f [substitute ti sigma | ti <- t]
addSubst :: Subst -> (String, Term) -> Maybe Subst 
addSubst subst (x, t) =
    case lookup x subst of 
        Just t' | t' /= t -> Nothing
        _                 -> Just ((x, t) : subst)
matchPairs :: [(Term, Term)] -> Subst -> Maybe Subst 
matchPairs [] subst                = Just subst
matchPairs ((V x, t) : pairs) subst = 
    case addSubst subst (x, t) of
        Just subst' -> matchPairs pairs subst'
        Nothing     -> Nothing
matchPairs ((F f t, F g u) : pairs) subst
    | f == g && length t == length u = matchPairs (zip t u ++ pairs) subst
    | otherwise                      = Nothing
matchPairs _ _ = Nothing
match :: Term -> Term -> Maybe Subst
match l t = matchPairs [(l, t)] []
reducts :: TRS -> Term -> [Term]
reducts rule t = 
    [ replace t (substitute r sigma) p |
      p <- positions t,
      (l, r) <- rule,
      Just sigma <- [match l (subtermAt t p)]]
rewrite :: TRS -> Term -> Maybe Term
rewrite rules t = case reducts rules t of
    (u : _) -> Just u
    []      -> Nothing
nf :: TRS -> Term -> Term
nf rules t = case rewrite rules t of
    Just u -> nf rules u
    Nothing -> t