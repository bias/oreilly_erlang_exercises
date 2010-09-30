-module(ex).
-export([p1_1/1, p1_2/2, p1_3/1, p1_4/1, p1_5/1, p2_1/0, p2_2/1, p2_3/2, p2_4/2]).

% Problem 1:
print_list(L) -> 
	lists:map(fun(X) -> io:format("~p ",[X]) end, L), ok.
 
p1_1(N) -> 
	print_list(lists:seq(1, N)).

p1_2(L,N) -> 
	lists:filter(fun(X) -> X =< N end, L). 

p1_3(N) -> 
	print_list(lists:filter(fun(X) -> X rem 2 == 0 end, lists:seq(1, N))).

p1_4(Lol) -> 
	lists:foldl(fun(L, AccL) -> AccL ++ L end, [], Lol).

p1_5(L) ->
	lists:foldl(fun(X, Acc) -> X + Acc end, 0, L).

% Problem 2:

p2_1() -> [X || X <- lists:seq(1,10), X rem 3 == 0].

p2_2(L) -> [X*X || X <- L, is_integer(X)].

p2_3(L1, L2) -> [Y || X <- L1, Y <- L2, X == Y].

p2_4(L1, L2) -> lists:filter(fun(X) -> not lists:member(X,p2_3(L1, L2)) end, L1 ++ L2). 
