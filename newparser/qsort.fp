leq(0,X)       = true
leq(s(X),0)    = false
leq(s(X),s(Y)) = leq(X,Y)

isort(nil) = nil
isort(cons(X,XS)) = insert(X,isort(XS))

list_zero(X)                    = list(0, X)
list(L, H)                      = list_from(L, H)
list_from(L, H)                 = list_when(leq(L, H), L, H)
list_when(false, _, _)          = nil
list_when(true,  L, L)          = cons(L, nil)
list_when(true,  L, s(H_prime)) = cons(s(H_prime), cons(L, list_from(s(L), H_prime)))

append(nil, YS) = YS
append(cons(X, XS), YS) = cons(X, append(XS, YS))
qsort(nil) = nil
qsort(cons(X, XS)) = split(X, XS, nil, nil)
split(W, nil, YS, ZS) = append(qsort(YS), cons(W, qsort(ZS)))
split(W, cons(X, XS), YS, ZS) = if_split(leq(W, X), W, X, XS, YS, ZS)
if_split(true, W, X, XS, YS, ZS) = split(W, XS, YS, cons(X, ZS))
if_split(false, W, X, XS, YS, ZS) = split(W, XS, cons(X, YS), ZS)

main = qsort(list_zero(30))