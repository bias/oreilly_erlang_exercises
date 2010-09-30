-module(db_sup).

-export([start_link/0]).
-export([init/1]).

-behavior(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	Db_Child = {my_db_gen, {my_db_gen, start, []}, permanent, 30000, worker, [db, my_db_gen]},
	{ok, {{one_for_one, 1, 1}, [Db_Child]}}.
