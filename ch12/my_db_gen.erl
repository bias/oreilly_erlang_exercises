-module(my_db_gen).

-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).

-behavior(gen_server).

%% Exported Client Functions
%% Operation & Maintenance API

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
	gen_server:cast(?MODULE, stop).

%% DataBase API (right now this is synchronous)

write(Key, Value) -> 
	gen_server:call(?MODULE, {write, Key, Value}). 

delete(Key) -> 
	gen_server:call(?MODULE, {delete, Key}).

read(Key) -> 
	gen_server:call(?MODULE, {read, Key}).

match(Value) -> 
	gen_server:call(?MODULE, {match, Value}).

%% Callback functions

init(_Args) ->
	{ok, [db:new()]}.	

terminate(_Reason, Db) ->
	db:destroy(Db).

handle_cast(stop, Db) ->
	{stop, normal, Db}.

handle_call({write, Key, Value}, _From, Db) ->
	{reply, ok, db:write(Key, Value, Db)};
handle_call({delete, Key}, _From, Db) ->
	{reply, ok, db:delete(Key, Db)};
handle_call({read, Key}, _From, Db) ->
	case db:read(Key, Db) of
		{ok, Value} -> 
			{reply, {ok, Value}, Db};
		{error, instance} -> 
			{reply, {error, instance}, Db}
	end;
handle_call({match, Value}, _From, Db) ->
	{reply, db:match(Value, Db), Db}.
