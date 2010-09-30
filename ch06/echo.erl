-module(echo).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
	register(echo, spawn_link(echo, loop, [])).

print(Term) ->
	echo ! {print, Term}.

stop() ->
	echo ! stop.

loop() ->
	receive
		{print, Term} -> 
			io:write(Term), 
			loop();
		stop -> ok
	end.
