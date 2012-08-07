-module(remote).

-compile(export_all).

-include("../include/meta.hrl").

meta_integer() ->
    ?q(42).

meta_inc() ->
    ?q(fun(A) -> A + 1 end).

meta_add(A, B) ->
    ?q(?sv(A) + ?sv(B)).
    
trans(A, B) ->
    meta_add(A, B).


%% hygenic_splice(_G, L, 0) ->
%%     begin
%%         ?q({?sv(L)})
%%     end;
%% hygenic_splice(G, L, N) ->
%%     ?q(begin
%%            Var = ?s(G) + ?sv(L),
%%            {Var, ?s(hygenic_splice(G, ?qv(Var), N-1))}
%%        end).

%% meta_var_test() ->
%%     E = 1,
%%     ?s(hygenic_splice(?qv(E), ?q(42), 2)).
