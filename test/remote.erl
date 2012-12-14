-module(remote).

-compile(export_all).

-include("../include/meta.hrl").

meta_integer() ->
    ?q(42).

meta_inc() ->
    ?q(fun(A) -> A + 1 end).

meta_add(A, B) ->
    ?q(?s(A) + ?s(B)).
    
trans(A, B) ->
    meta_add(A, B).
