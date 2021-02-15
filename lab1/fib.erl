-module(fib).

-export([fib_p/1, fib_g/1, tail_fib/1]).

fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1) + fib_p(N-2).

fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) when N > 1  -> fib_g(N-1) + fib_g(N-2).

tail_fib(N) -> tail_fib(N, 0, 1).
tail_fib(0, Curr, Next) -> Curr;
tail_fib(N, Curr, Next) -> tail_fib(N - 1, Next, Curr + Next).
