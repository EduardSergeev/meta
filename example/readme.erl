-module(readme).

-include("../include/meta.hrl").

-compile(export_all).

%% Uncomment this to see splices expansion
%% -meta_opts([dump_splices]).

get_quote() ->
    ?q(1 + 2).

use_quote() ->
    ?s(get_quote()) + 39.


get_nested_quote(AnotherQuote) ->
    ?q(1 + ?s(AnotherQuote)).

use_nested_quote() ->
    ?s(get_nested_quote(?q(2 + 3))) + 37.

%% This produces "Warning: this expression will fail with a 'badarith' exception"
%% create_warning() ->
%%    ?s(get_nested_quote(?q(undefined))) + 37.

inc(Arg) ->
    Arg + 1.

triple_inc1(Arg) ->
    app_iter(3, Arg).
    
app_iter(0, Arg) ->
    Arg;
app_iter(N, Arg) ->
    app_iter(N-1, inc(Arg)).


triple_inc2(Arg) ->
    lists:foldl(
      fun(_, A) -> inc(A) end,
      Arg, lists:seq(1,3)).


triple_inc3(Arg) ->
    ?s(q_iter(3, ?r(Arg))).
    
q_iter(0, QArg) ->
    QArg;
q_iter(N, QArg) ->
    ?q(inc(?s(q_iter(N-1, QArg)))).

triple_inc4(Arg) ->
    ?s(lists:foldl(
         fun(_, Q) -> ?q(inc(?s(Q))) end,
         ?r(Arg), lists:seq(1,3))).


q_inc(Arg) ->
    ?q(?s(Arg) + 1).

triple_inc5(Arg) ->
    ?s(lists:foldl(
         fun(_, Q) -> q_inc(Q) end,
         ?r(Arg), lists:seq(1,3))).
