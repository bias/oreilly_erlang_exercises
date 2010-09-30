-module(ex).
-export([test/0]).

-ifdef(show).
	-define(SHOW_EVAL(Expr), apply( fun() -> io:format("~p = ~p~n", [??Expr, Expr]), Expr end, [])).
-else.
	-define(SHOW_EVAL(Expr),Expr).
-endif.

test() -> ?SHOW_EVAL(lists:sort([3,2,4,1])), ok.
