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
    [{"Simple type quote with splice argiment",
      ?_test(
         begin
             A = meta:quote(1), Q1 = meta:quote(meta:splice(A) + 2), Q2 = meta:quote(1 + 2),
             ?assertEqual(Q2, Q1)
         end)}].

%%
%% Local function call in 'meta:splice'
%%
local() ->
    meta:quote(42.2).

trans() ->
    local().

id(A) ->
    A.

local(A) ->
    meta:quote(meta:splice(A) + 1).

local(Fun, A) ->
    meta:quote((meta:splice(Fun))(meta:splice(A))).


local_call_test_() ->
    [?_assertEqual(42.2,
                   meta:splice(local())),
     ?_assertEqual(42.2,
                   meta:splice(trans())),
     ?_assertEqual(42.2,
                   meta:splice(id(id(trans())))),
     ?_assertEqual(3,
                   meta:splice(local(meta:quote(2)))),
     ?_test(
        begin
            R = meta:splice(
                  local(
                    meta:quote(fun(A) -> A + 3 end),
                    meta:quote(2))),
            ?assertEqual(5, R)
        end)].

%%
%% Remote function call in 'meta:splice'
%%
remote_call_test_() ->
    [?_assertEqual(42,
                   meta:splice(remote:meta_integer())),
     {"Automatic splice with '-meta' attribute",
      ?_assertEqual(42,
                    meta_add(meta:quote(21), meta:quote(21)))},
     {"Call via '-import' attribute",
      ?_test(
         begin
             Inc = meta:splice(meta_inc()),
             ?assert(is_function(Inc)),
             ?assertEqual(3, Inc(2))
         end)},
     ?_assertEqual(12.5,
                   meta:splice(
                     remote:trans(
                       meta:quote(11),
                       meta:quote(1.5))))].

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
