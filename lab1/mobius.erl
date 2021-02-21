-module(mobius).

-export([is_prime/1, prime_factors/1, is_square_multiple/1, find_square_multiples/2]).

% finds if the number is prime
is_prime(N) -> is_prime_help(N, trunc(math:sqrt(N))).

is_prime_help(1, _) -> true;
is_prime_help(2, _) -> true;
is_prime_help(_, 1) -> true; % termination condition
is_prime_help(Divisor, N) when Divisor rem N =:= 0
-> false;
is_prime_help(Divisor, N) when Divisor rem N =/= 0
-> is_prime_help(Divisor, N-1).

% gets list of prime factors
prime_factors(N) -> prime_factors_help(N, [], 2).

prime_factors_help(1, [], _) -> [1];
prime_factors_help(N, Result, N) -> Result ++ [N];
prime_factors_help(N, Result, M) when N rem M =/= 0
    -> prime_factors_help(N, Result, M+1);
prime_factors_help(N, Result, M) when N rem M =:= 0
    -> prime_factors_help(N div M, Result ++ [M] , 2).

% % checks if list of prime factors contains duplicate values 
is_square_multiple(N) ->
    PrimeFactorsList = prime_factors(N),
    erlang:length(PrimeFactorsList) =/= sets:size(sets:from_list(PrimeFactorsList)).

% 
% Count - длину последовательности чисел делящихся на квадрат простого числа, 
% MaxN - максимальное значение, после которого надо прекратить поиск.
find_square_multiples(Count, MaxN) ->
    find_square_helper(Count, MaxN, []).

find_square_helper(Count, ToTest, Found) when erlang:length(Found) == Count  -> 
    ToTest+1;
find_square_helper(_, 2, _) ->
    fail; % search failed
find_square_helper(Count, ToTest, Found) ->
    case is_square_multiple(ToTest) of 
        true -> NewFound = Found ++ [ToTest];
        _ -> NewFound = [] % reset
    end,
    find_square_helper(Count, ToTest-1, NewFound).