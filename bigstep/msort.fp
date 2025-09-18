leq(0,X)       = true
leq(s(X),0)    = false
leq(s(X),s(Y)) = leq(X,Y)

list_zero(X)                    = list(0, X)
list(L, H)                      = list_from(L, H)
list_from(L, H)                 = list_when(leq(L, H), L, H)
list_when(false, _, _)          = nil
list_when(true,  L, L)          = cons(L, nil)
list_when(true,  L, s(H_prime)) = cons(s(H_prime), cons(L, list_from(s(L), H_prime)))

split(nil)                    = pair(nil, nil)
split(cons(X, nil))           = pair(cons(X, nil), nil)
split(cons(X, cons(Y, ZS)))   = split_aux(X, Y, split(ZS))
split_aux(X, Y, pair(XS, YS)) = pair(cons(X, XS), cons(Y, YS))
merge(nil, nil)                           = nil
merge(XS, nil)                            = XS
merge(nil, YS)                            = YS
merge(cons(X, XS), cons(Y, YS))           = if_merge(leq(X, Y), cons(X, XS), cons(Y, YS))
if_merge(true, cons(X, XS), cons(Y, YS))  = cons(X, merge(XS, cons(Y, YS)))
if_merge(false, cons(X, XS), cons(Y, YS)) = cons(Y, merge(cons(X, XS), YS))
msort(nil)              = nil
msort(cons(X, nil))     = cons(X, nil)
msort(XS)               = msort_aux(split(XS))
msort_aux(pair(YS, ZS)) = merge(msort(YS), msort(ZS))

main = msort(list_zero(100))
