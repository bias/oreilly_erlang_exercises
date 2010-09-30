-module(peer).
-export([start/0, stop/0, connect/1, send/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

%% Operation API

start() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop). 

%% Service API

connect(Ip) -> 
	gen_server:call(?MODULE, {connect, Ip}, 10000). 

send(String) -> 
	gen_server:cast(?MODULE, {send, String}).

%% Callback Functions

init(_Args) ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, inet]),
	io:format("Listening on socket ~p~n", [ListenSocket]),
	{ok, {ListenSocket,null}}.

terminate(_Reason, null) -> ok;
terminate(_Reason, {ListenSocket,SendSocket}) ->
	gen_tcp:close(ListenSocket),
	gen_tcp:close(SendSocket).

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData};

handle_cast({send, String}, LoopData={_ListenSocket,SendSocket}) ->
	% FIXME we're not regulating packet size!
	Reply = gen_tcp:send(SendSocket, String),
	{reply, Reply, LoopData}.

handle_call({connect, Ip}, _From, {ListenSocket,_SendSocket}) ->
	io:format("Handling connect to ~p~n", [Ip]), 
	{ok, NewSocket} = gen_tcp:connect(Ip, 1234, [binary, inet]),
	{reply, {ok, NewSocket}, {ListenSocket, NewSocket}}.
