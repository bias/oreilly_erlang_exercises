-module(das).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([loop/1]).

start() ->
	register(?MODULE, spawn(?MODULE, loop, [db:new()])), ok.

stop() -> 
	?MODULE ! {stop, self()}, ok.

write(Key, Value) -> 
	remote_call(main@localhost, {write, Key, Value}),
	remote_call(backup@localhost, {write, Key, Value}).

delete(Key) -> 
	remote_call(main@localhost, {delete, Key}),
	remote_call(backup@localhost, {delete, Key}).

read(Key) -> 
	{_,_,T} = now(),
	if 
		T rem 2 == 0 -> 
			remote_call(main@localhost, {read, Key});
		true -> 
			remote_call(backup@localhost, {read, Key})
	end.

match(Value) -> 
	{_,_,T} = now(),
	if 
		T rem 2 == 0 -> 
			remote_call(main@localhost, {match, Value});
		true -> 
			remote_call(backup@localhost,{match, Value})
	end.

handle_msg({write, Key, Value}, Db) ->
	{ok, db:write(Key, Value, Db)};
handle_msg({delete, Key}, Db) ->
	{ok, db:delete(Key, Db)};
handle_msg({read, Key}, Db) ->
	case db:read(Key, Db) of
		{ok, Value} -> 
			{{ok, Value}, Db};
		{error, instance} -> 
			{{error, instance}, Db}
	end;
handle_msg({match, Value}, Db) ->
	{db:match(Value, Db), Db}.

remote_call(Node, Msg) ->
	{?MODULE, Node} ! {request, self(), Msg},
	receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
	To ! {reply, Msg}.

loop(State) ->
	receive
		{request, From, Msg} -> 
			{Reply, NewState} = handle_msg(Msg, State),
			reply(From, {Reply,self()}),
			loop(NewState);
		{stop, _From} -> 
			db:destroy(State)
	end.
