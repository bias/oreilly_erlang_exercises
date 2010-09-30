-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% Interface:
% 	new()                     => Db.
% 	destroy(Db)               => ok.
% 	write(Key, Element, Db)   => NewDb.
% 	delete(Key, Db)           => NewDb.
% 	read(Key, Db)             => {ok, Element} | {error, instance}
% 	match(Element, Db)        => [key1, ..., keyN]

new() -> []. 

destroy(_) -> ok.

write(Key, Element, Db) -> 
	[{Key, Element}|Db]. 

delete(Key, [{Key,_}|Tail]) ->
	Tail;
delete(Key, [Head|Tail]) ->
	[Head|delete(Key,Tail)].

read(Key, [{Key,Element}|_]) -> 
	{ok, Element};
read(Key, [_|Tail]) ->
	read(Key, Tail);
read(_, []) ->
	{error, instance}.

match(Element, [{Key,Element}|Tail]) -> 
	[Key|match(Element,Tail)];
match(Element, [_|Tail]) ->
	match(Element, Tail);
match(_, []) ->
	[].
