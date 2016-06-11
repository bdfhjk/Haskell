FN (I=>I) fib(I x) = 
    if x == 1 
    then 1 else 
        if x == 2 
        then 1 
        else ( call fib(x-1) + call fib(x-2))
-> call fib(15)
