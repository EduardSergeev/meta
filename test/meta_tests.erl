%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(meta_tests).

-include("../include/meta.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% auto-spliced local function
-meta([meta_local/1]).
%% auto-spliced local operator
-meta(['=<'/2]).

-import(remote, [meta_inc/0, meta_add/2]).
-meta([meta_add/2]).


%%
%% meta:quote tests
%%
q1() ->
    [meta:quote(1),
     meta:quote(1.1),
     meta:quote(true),
     meta:quote(atom),
     meta:quote(<<"Bin">>),
     meta:quote({1,{[2,3],true}})].

q2() ->
    [meta:quote(fun() -> 42 end),
     meta:quote(fun q1/0),
     meta:quote(fun(A) -> A + 1 end)].

quote_test_() ->
    [{"Basic type quotes",
      ?_assert(lists:all(fun is_valid_quote/1, q1()))},
     {"Function type quotes",
      ?_assert(lists:all(fun is_valid_quote/1, q2()))}].

splice_test_() ->
    [{"Basic type splices",
      ?_test(
         [meta:splice(hd(q1())),
          meta:splice(lists:nth(2, q1())),
          meta:splice(lists:nth(3, q1())),
          meta:splice(lists:nth(4, q1())),
          meta:splice(lists:nth(5, q1())),
          meta:splice(lists:last(q1()))])},
     {"Inline splice",
      [?_assertEqual(42,
                     meta:splice(meta:quote(42))),
       ?_assertEqual(42,
                     (meta:splice(meta:quote(fun(A) -> A end)))(42))]},
     {"Function type slices",
      ?_test(
         begin
             F1 = meta:splice(hd(q2())),
             ?assertEqual(42, F1()),
             F2 = meta:splice(lists:nth(2, q2())),
             ?assertEqual(q1(), F2()),
             F3 = meta:splice(lists:nth(3, q2())),
             ?assertEqual(3, F3(2))
         end)}].
       
quote_splice_test_() ->
    [{"Simple type quote with splice argument",
      ?_test(
         begin
             A = ?q(1),
             Q1 = ?q(?s(A) + 2),
             Q2 = ?q(1 + 2),
             ?assertEqual(Q2, Q1)
         end)},
     {"Anihilaation of quote(splice()) pairs",
      [?_assertEqual(42, ?q(?s(42))),
       ?_assertEqual(42, ?s(?q(42))),
       ?_assertEqual(?q(42), ?q(?s(?q(42)))),
       ?_assertEqual(42, ?s(?q(?s(?q(42))))),
       {"local -meta function in quote",
        ?_assertEqual(?q(42 + 1), ?q(meta_local(42)))},
       {"remote -meta function in quote",
        ?_assertEqual(?q(42 + 42.0), ?q(meta_add(42, 42.0)))}]}].

%%
%% Local function call in 'meta:splice'
%%
local() ->
    ?q(42.2).

trans() ->
    local().

id(A) ->
    A.

local(A) ->
    ?q(?s(A) + 1).

local(Fun, A) ->
    ?q(?s(Fun)(?s(A))).

recursive(0) ->
    ?q(0);
recursive(N) ->
    A = N,
    B = id(N),
    ?q({?s(erl_parse:abstract(A)), ?s(recursive(B-1))}).

%% This one is auto-spliced via '-meta' attribute
meta_local(QN) ->
    ?q(?s(QN) + 1).

%% -meta operator: meta-converts into fun call
'=<'(QLeft, QRight) ->
    ?q(?s(QLeft)(?s(QRight))).


local_call_test_() ->
    [?_assertEqual(42.2, ?s(local())),
     ?_assertEqual(42.2, ?s(trans())),
     ?_assertEqual(42.2, ?s(id(id(trans())))),
     ?_assertEqual(3, ?s(local(?q(2)))),
     ?_test(
        begin
            R = ?s(local(
                     ?q(fun(A) -> A + 3 end),
                     ?q(2))),
            ?assertEqual(5, R)
        end),
     ?_assertEqual({3,{2,{1,0}}}, ?s(recursive(3))),
     ?_assertEqual(2, meta_local(1)),
     {"-meta operator call",
      ?_assertEqual(2, size =< <<"42">>)},
     {"nested -meta operator",
      ?_assertEqual(1, length =< integer_to_list(size =< <<"42">>))}].

%%
%% Remote function call in 'meta:splice'
%%
remote_call_test_() ->
    [?_assertEqual(42, ?s(remote:meta_integer())),
     {"Automatic splice with '-meta' attribute",
      ?_assertEqual(42, meta_add(21, 21))},
     {"Call via '-import' attribute",
      ?_test(
         begin
             Inc = ?s(meta_inc()),
             ?assert(is_function(Inc)),
             ?assertEqual(3, Inc(2))
         end)},
     ?_assertEqual(12.5, ?s(remote:trans(?q(11), ?q(1.5))))].

%%
%% quote manipulation test
%%
meta_call(F, A) ->
    ?q(?s(F)(?s(A))).

unwind(N, QFun, QArg) ->
    F = fun(E, A) ->
                meta_call(E, A)
        end,
    lists:foldr(F, QArg, lists:duplicate(N, QFun)).

p1(A) ->
    A + 1.

unwind_fun_test() ->
    QFunName = ?q(p1),
    QArg = ?q(42),
    N = 3,
    QUnwinded = unwind(N, QFunName, QArg),
    ?assertEqual("p1(p1(p1(42)))",
                 erl_prettypr:format(QUnwinded)),
    ?assertEqual(6, ?s(unwind(5, ?q(p1), ?q(1)))).
    

%%
%% Utilities
%%
is_valid_quote(QExpr) ->
    try
        erl_lint:exprs([QExpr], []),
        true
    catch error:_ ->
            false
    end.
