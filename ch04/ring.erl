-module(ring).
-export([start/2, message/1, stop/0, root/3, node/4]).

start(M,N) -> 
	register(ring_root, spawn(ring, root, [M,N-1,self()])).
	
message(M) -> 
	ring_root ! {message, start, M}.

stop() -> 
	ring_root ! stop.

root(M,N,Shell_Pid) ->
	NPid = spawn(ring, node, [M,N-1,self(),self()]),
	listening(N,M,NPid).

node(M,0,Pid,Root) -> 
	listening(0,M,Root);
node(M,N,Pid,Root) ->
	NPid = spawn(ring, node, [M,N-1,self(),Root]),
	listening(N,M,NPid).

listening(N,0,Pid) -> 
	Pid ! stop;
listening(N,M,Pid) ->
	receive
		stop -> Pid ! stop;
		{message, 0, Message} -> 
			Pid ! listening(N,M,Pid);
		{message, start, Message} -> 
			Pid ! {message, N-1, Message}, 
			listening(N,M-1,Pid);
		{message, N, Message} -> 
			Pid ! {message, N-1, Message}, 
			listening(N,M-1,Pid)
	end. 
