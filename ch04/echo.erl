-module(echo).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
	register(echo, spawn(echo, loop, [])).

print(Term) ->
	echo ! {print, Term}.

stop() ->
	echo ! stop.

% I feel really weird about having to export this ...
loop() ->
	receive
		{print, Term} -> 
			io:write(Term), 
			loop();
		stop -> ok
	end.
