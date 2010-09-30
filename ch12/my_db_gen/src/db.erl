-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

-include("pair.hrl").

% Debuging macro
-ifdef(debug).
	-define(SHOW_EVAL(Expr), apply( fun() -> io:format("~p = ~p~n", [??Expr, Expr]), Expr end, [])).
	-define(REPORT(String, List), io:format(String, List)).
-else.
	-define(SHOW_EVAL(Expr), Expr).
	-define(REPORT(String, List), ok).
-endif.

new() -> 
	[]. 

destroy(_Db) -> 
	ok.

write(Key, Value, Db) -> 
	?REPORT("Writing pair=~p~n", [{Key,Value}]),
	[#pair{key=Key, value=Value} | Db]. 

delete(Key, [#pair{key=Key}|Tail]) ->
	?REPORT("Deleting pair with key=~p~n", [Key]),
	Tail;
delete(Key, [Head|Tail]) ->
	[Head|delete(Key,Tail)];
delete(_,[]) -> 
	[].

read(Key, [#pair{key=Key,value=Value}|_]) -> 
	?REPORT("Reading key=~p with value=~p~n", [Key,Value]),
	{ok, Value};
read(Key, [_|Tail]) ->
	read(Key, Tail);
read(_, []) ->
	{error, instance}.

match(Value, [#pair{key=Key,value=Value}|Tail]) -> 
	?REPORT("Matching on value=~p with key=~p~n", [Value,Key]),
	[Key|match(Value,Tail)];
match(Value, [_|Tail]) ->
	match(Value, Tail);
match(_, []) ->
	[].
