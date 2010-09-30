-module(boolean).
-export([b_not/1,b_and/2,b_or/2,b_nand/2]).

% Return logical results using atoms true and false
% using pattern matching instead of logical control.

b_nand(true, true) -> false;
b_nand(true, false) -> true;
b_nand(false, true) -> true;
b_nand(false, false) -> true.

b_not(X) -> b_nand(X, X).

b_and(X, Y) -> b_nand( b_nand(X, Y), b_nand(X, Y)).

b_or(X, Y) -> b_nand( b_nand(X, X), b_nand(Y, Y)).
