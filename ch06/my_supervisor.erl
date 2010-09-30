-module(my_supervisor).
-export([start_link/1, start_child/3, stop_child/1, stop/0]).
-export([init/1]).

start_link(ChildSpecList) ->
  register(my_supervisor, spawn_link(?MODULE, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  timer:start(),
  loop(start_children(ChildSpecList, 0), []).

start_children([], _) -> [];
start_children([{M, F, A, T} | ChildSpecList], N) ->
  case (catch apply(M, F, A)) of
    {ok, Pid} ->
      [ {Pid, N+1, {M, F, A, T}} | start_children(ChildSpecList, N+1)]
	% I removed case where bad apply return is ignored (otherwise we have a loose child)!
	% This also prevents the infinite restart case for unavailble modules (but not gracefully).
  end.

start_child(M, F, A) ->
  my_supervisor ! {start, {M,F,A}, self()}, 
  receive
  	{reply, {Pid, Id, _T}} -> {Id, Pid} 
  end.

stop_child(Id) ->
  my_supervisor ! {stop, id, Id, self()}, 
  receive
  	{reply, Reply} -> ok
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages. 
%% If a child terminates the supervisor receives the EXIT signal two events occur depending on the Type parameter:
%% a permanent child is restarted and its entry in the list of children stored in the ChildList variable is replaced;
%% a transient child is allowed to die.
opt_restart_child(Pid, ChildList, RestartList) ->
  case lists:keysearch(Pid, 1, ChildList) of
    {value, {Pid, N, {M, F, A, permanent}}} ->
	  update_restart_count({Pid, N, {M,F,A,permanent}}, ChildList, RestartList);
	{value, {Pid, _N, {_M, _F, _A, transient}}} ->
	  {ChildList, RestartList}
  end.

update_restart_count({Pid, N, {M,F,A,T}}, ChildList, RestartList) ->
	  case lists:keyfind({M,F}, 1, RestartList) of
        false -> 
		  io:format("Restart count 1 for ~p~n", [{M,F}]),
		  timer:send_after(60000, {timer, {M,F}}),
		  {restart_child({Pid, N, {M,F,A,T}}, ChildList), [ {{M,F},1} | RestartList ]};
		{{M,F}, 5} ->
		  io:format("Max of 5 restarts per min reached for ~p~n", [{M,F}]),
          {lists:keydelete({M,F,A}, 2, ChildList), RestartList};
		{{M,F}, C} ->
		  io:format("Restart count ~p for ~p~n", [C+1,{M,F}]),
		  lists:keydelete({M,F}, 1, RestartList),
	      {restart_child({Pid, N, {M,F,A,T}}, ChildList), lists:keyreplace({M,F}, 1, RestartList, {{M,F},C+1})}
	  end.

restart_child({Pid, N, {M,F,A,T}}, ChildList) ->
  {ok, NewPid} = apply(M, F, A),
  [ {NewPid, N+1, {M, F, A, T}} | lists:keydelete(Pid,1,ChildList) ].

loop(ChildList, RestartList) ->
  receive
    {start, {M,F,A}, From} ->
	  {_,N,_T} = lists:last( lists:keysort(2, ChildList) ),
	  [NewChild] = start_children([{M,F,A,permanent}],N),
	  From ! {reply, NewChild},
	  loop([NewChild | ChildList], RestartList);
    {timer, {M,F}} ->
	  io:format("Timer rest called for ~p~n", [{M,F}]),
	  NewRestartList = lists:keydelete({M,F}, 1, RestartList),
	  loop(ChildList, NewRestartList);
    {'EXIT', Pid, _Reason} ->
	  io:format("Child: ~p  Restart: ~p~n", [ChildList, RestartList]),
      {NewChildList, NewRestartList} = opt_restart_child(Pid, ChildList, RestartList),
	  io:format("New Child: ~p  New Restart: ~p~n", [NewChildList, NewRestartList]),
      loop(NewChildList, NewRestartList);
    {stop, From}  ->
      From ! {reply, terminate(ChildList)};
    {stop, id, Id, From} ->
	  {Pid, _Id, _T} = lists:keyfind(Id, 2, ChildList),
	  NewChildList = lists:keydelete(Id, 2, ChildList),
	  From ! {reply, exit(Pid, kill)},
	  receive {'EXIT', Pid, _Reason} -> ok end,
	  loop(NewChildList, RestartList)
  end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the 
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated 
%% the stop call:
stop() ->
  my_supervisor ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _Id, _T} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
