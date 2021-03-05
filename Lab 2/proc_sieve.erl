%% compile c(proc_sieve).
%% run proc_sieve:gen_print(500).
-module(proc_sieve).

-export([generate/1, sieve_run/2]).
-export([gen_print/1]).

-define(TIMEOUT, 100000).

generate(MaxN) ->
    Pid = sieve(),
    generate_help(Pid, 2, MaxN). 

generate_help(Pid, End, End) ->
    Pid ! {done, self()}, %% send final msg
    receive
            Res -> Res, %% result list of primes
            lists:foreach(
                fun(N) -> 
                    io:format("~w~n",  [N]) end, Res %% print each prime
            )
    end;

generate_help(Pid, N, End) ->
    Pid ! N, %% send msg with next number to process
    generate_help(Pid, N + 1, End).

sieve() ->
    spawn(proc_sieve, sieve_run, [0, void]). %%spawn(module, function, args)

sieve_run(0, InvalidPid) ->
    receive 
        P -> sieve_run(P, InvalidPid)
    after ?TIMEOUT ->
        io:format("Timeout in P=0~n")
    end;

%% starting condition
sieve_run(P, NextPid) when is_pid(NextPid) ->
    receive 
        {done, From} ->
            NextPid ! {done, self()},
            receive 
                ListOfRes -> 
                    From ! [P] ++ ListOfRes 
            end;
        N when N rem P == 0 -> 
            sieve_run(P, NextPid);
        N when N rem P /= 0 -> 
            NextPid ! N,
            sieve_run(P, NextPid)
    after ?TIMEOUT ->
        io:format("Timeout in is_pid clause P=~p~n", [P])
    end;

sieve_run(P, Invalid) ->
    receive 
        {done, From} ->
            %% no downstream process, just send the result back 
            From ! [P];
        N when N rem P == 0 -> 
            sieve_run(P, Invalid);
        N when N rem P /= 0 -> 
            % io:format("Starting ~p for ~p~n", [self(), N]), 
            Pid = spawn(proc_sieve, sieve_run, [0, void]),
            Pid ! N,
            sieve_run(P, Pid)
    after ?TIMEOUT ->
        io:format("Timeout in no pid clause P=~p~n", [P])
    end. 

gen_print(MaxN) ->
    generate(MaxN).
