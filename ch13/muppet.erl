-module(muppet).
-export([pay_raise/1]).

-record(muppet, {name, callsign, salary}).

pay_raise(#muppet{salary=Salary} = M) -> 
	mnesia:transaction(fun() -> mnesia:write(M#muppet{salary=Salary*1.1}) end).
