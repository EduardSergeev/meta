-module(remote).

-compile(export_all).

-include("../include/meta.hrl").

meta_integer() ->
    meta:quote(42).

meta_inc() ->
    meta:quote(fun(A) -> A + 1 end).

meta_add(A, B) ->
    meta:quote(meta:splice(A) + meta:splice(B)).
    

trans(A, B) ->
    meta_add(A, B).

    %% Add = fun(A,B) ->
    %%               meta:splice(meta_add(meta:quote(A), meta:quote(B)))
    %%       end,
    %% Add(40,2).
                  
