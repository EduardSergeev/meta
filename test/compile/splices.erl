-module(splices).

-compile(export_all).

-include("../../include/meta.hrl").

%% splice_base() ->
%%     meta:splice(meta:quote(42)).

%% integer() ->
%%     meta:quote(42).

recursive(0) ->
    ?q(0);
recursive(N) ->
    A = N,
    ?q({?s(fun(V) -> {erl_parse:abstract(A), V} end), ?s(recursive(A-1))}).

gen(N) ->
    N.

test() ->
    ?s(recursive(5)).
    
