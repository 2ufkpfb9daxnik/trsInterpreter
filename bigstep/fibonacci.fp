add(0, Y) = Y
add(s(X), Y) = s(add(X, Y))

fibonacci(0) = 0
fibonacci(1) = 1
fibonacci(s(s(N))) = add(fibonacci(s(N)), fibonacci(N))

main = fibonacci(20)