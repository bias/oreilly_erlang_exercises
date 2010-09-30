-module(peer).
-export([start/0, client/2, wait_connect/2, get_request/3, handle/2]).

start() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	spawn_link(?MODULE, wait_connect, [ListenSocket, 0]).  	

client(Host, Data) ->
    {ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
    gen_tcp:send(Socket, Data),
    ok = gen_tcp:close(Socket).

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	Pid = spawn(?MODULE, get_request, [Socket, [], Count]),
	gen_tcp:controlling_process(Socket, Pid),
	wait_connect(ListenSocket, Count+1).


get_request(Socket, BinaryList, Count) ->
    case gen_tcp:recv(Socket, 0, 5000) of
	{ok, Binary} ->
	    get_request(Socket, [Binary|BinaryList], Count);
	{error, closed} ->
	    handle(lists:reverse(BinaryList), Count)
    end.


handle(Binary, Count) ->
    {ok, Fd} = file:open("log_file_"++integer_to_list(Count), write),
    file:write(Fd, Binary),
    file:close(Fd).

