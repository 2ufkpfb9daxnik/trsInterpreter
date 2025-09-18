leq(0,X)       = true
leq(s(X),0)    = false
leq(s(X),s(Y)) = leq(X,Y)

insert(X,nil)           = cons(X,nil)
insert(X,cons(Y,YS))    = if_insert(leq(X,Y),X,Y,YS)
if_insert(true,X,Y,YS)  = cons(X,cons(Y,YS))
if_insert(false,X,Y,YS) = cons(Y,insert(X,YS))

isort(nil) = nil
isort(cons(X,XS)) = insert(X,isort(XS))

list_zero(X)                    = list(0, X)
list(L, H)                      = list_from(L, H)
list_from(L, H)                 = list_when(leq(L, H), L, H)
list_when(false, _, _)          = nil
list_when(true,  L, L)          = cons(L, nil)
list_when(true,  L, s(H_prime)) = cons(s(H_prime), cons(L, list_from(s(L), H_prime)))

main = isort(list_zero((40)))