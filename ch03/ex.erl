-module(ex).
-export([sum/1, sum/2, create/1, reverse_create/1, print_ints/1, print_even_ints/1]).

% Exercise 1
sum(N) when N >= 0 -> N*(N+1)/2.

sum(N,M) when N =< M -> sum(M) - sum(N).

% Exercise 2
reverse_create(0) -> [];
reverse_create(N) when N >= 0 -> [ N | reverse_create(N-1) ].

create(N) -> create_(N,0).
create_(N,N) -> [];
create_(N,C) -> [ C+1 | create_(N,C+1) ].

% Exercise 3
print_ints(N) -> print_ints_(N,1).
print_ints_(N,C) when C > N -> ok;
print_ints_(N,C) ->
	io:format("Number: ~p~n", [C]),
	print_ints_(N,C+1).

print_even_ints(N) -> print_even_ints_(N,2).
print_even_ints_(N,C) when C >= N -> ok;
print_even_ints_(N,C) ->
	io:format("Number: ~p~n", [C]),
	print_even_ints_(N,C+2).
