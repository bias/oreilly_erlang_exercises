-module(fac).
-export([call/1]).

call(X) ->
    {any, 'cnode@192.168.1.100'} ! {self(), X},
    receive
        {ok, Result} ->
            Result;
		M -> M
    end.
