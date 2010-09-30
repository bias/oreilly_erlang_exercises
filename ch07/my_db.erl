-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([loop/1]).

start() ->
	register(my_db, spawn(my_db, loop, [db:new()])), ok.

stop() -> 
	my_db ! {stop, self()}, ok.

write(Key, Value)    -> call(my_db, {write, Key, Value}).
delete(Key)            -> call(my_db, {delete, Key}).
read(Key)              -> call(my_db, {read, Key}).
match(Value)         -> call(my_db, {match, Value}).

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

call(Name, Msg) ->
	Name ! {request, self(), Msg},
	receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
	To ! {reply, Msg}.

loop(State) ->
	receive
		{request, From, Msg} -> 
			{Reply, NewState} = handle_msg(Msg, State),
			reply(From, Reply),
			loop(NewState);
		{stop, _From} -> 
			db:destroy(State)
	end.
