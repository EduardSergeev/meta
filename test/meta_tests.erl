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
    [?q(1),
     ?q(1.1),
     ?q(true),
     ?q(atom),
     ?q(<<"Bin">>),
     ?q({1,{[2,3],true}})].

q2() ->
    [?q(fun() -> 42 end),
     ?q(fun q1/0),
     ?q(fun(A) -> A + 1 end)].

quote_test_() ->
    [{"Basic type quotes",
      ?_assert(lists:all(fun is_valid_quote/1, q1()))},
     {"Function type quotes",
      ?_assert(lists:all(fun is_valid_quote/1, q2()))}].

v1() ->
    [?v(erl_syntax:revert(erl_syntax:abstract(42))),
     ?v(erl_syntax:revert(erl_syntax:abstract(atom)))].

v2() ->
    [?v(erl_syntax:revert(
          erl_syntax:fun_expr(
            [hd(erl_syntax:fun_expr_clauses(
                  ?s(?q(fun(A, B) ->
                                {A, B}
                        end))))])))].

verbatim_clash() ->
    AVar = erl_syntax:revert(erl_syntax:variable('A')),
    QVar = ?v(AVar),
    ?q(begin
           A = 41,
           ?s(QVar) + 1
       end).

verbatim_test_() ->
    [{"Basic verbatim",
      ?_assert(lists:all(fun is_valid_quote/1, v1()))},
     {"Function type verbatims",
      ?_assert(lists:all(fun is_valid_quote/1, v2()))},
     {"Non-hygienic verbatim splice",
      ?_assertMatch(42, ?s(verbatim_clash()))}].
    


splice_test_() ->
    [{"Basic type splices",
      ?_test(
         [?s(hd(q1())),
          ?s(lists:nth(2, q1())),
          ?s(lists:nth(3, q1())),
          ?s(lists:nth(4, q1())),
          ?s(lists:nth(5, q1())),
          ?s(lists:last(q1()))])},
     {"Inline splice",
      [?_assertEqual(42,
                     ?s(?q(42))),
       ?_assertEqual(42,
                     (?s(?q(fun(A) -> A end)))(42))]},
     {"Function type slices",
      ?_test(
         begin
             F1 = ?s(hd(q2())),
             ?assertEqual(42, F1()),
             F2 = ?s(lists:nth(2, q2())),
             ?assertEqual(q1(), F2()),
             F3 = ?s(lists:nth(3, q2())),
             ?assertEqual(3, F3(2))
         end)}].
       
quote_splice_test_() ->
    [{"Simple type quote with splice argument",
      ?_test(
         begin
             A = ?q(1),
             Q1 = ?q(?s(A) + 2),
             Q2 = ?q(1 + 2),
             ?assertEqual(Q2(gb_sets:new()), Q1(gb_sets:new()))
         end)},
     {"Anihilaation of quote(splice()) pairs",
      [?_assertEqual(42, ?s(?q(42))),
       ?_assertEqual(?e(?q(42)),
                     ?e(?q(?s(?q(42))))),
       ?_assertEqual(42, ?s(?q(?s(?q(42))))),
       {"local -meta function in quote",
        ?_assertEqual((?q(42 + 1))(gb_sets:new()),
                      (?q(meta_local(42)))(gb_sets:new()))},
       {"remote -meta function in quote",
        ?_assertEqual((?q(42 + 42.0))(gb_sets:new()),
                      (?q(meta_add(42, 42.0)))(gb_sets:new()))}]},
     {"Verbatim tests",
      [?_assertEqual(?e(?q(?s(?v(erl_syntax:revert(erl_syntax:abstract(42)))) + 1)),
                     ?e(?q(42 + 1)))]}].

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
    ?q({?s(fun(V) -> {erl_parse:abstract(A), V} end), ?s(recursive(B-1))}).

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
      ?_assertEqual(1, length =< integer_to_list(size =< <<"42">>))}
].

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
    {QUnwinded,_} = (unwind(N, QFunName, QArg))(gb_sets:new()),
    ?assertEqual("p1(p1(p1(42)))",
                 erl_prettypr:format(QUnwinded)),
    ?assertEqual(6, ?s(unwind(5, ?q(p1), ?q(1)))).
    

%%
%% Hygienic meta-variables  
%%
hygenic_splice(G, L, 0) ->
    begin
        ?q(?s(G) + ?s(L))
    end;
hygenic_splice(G, _L, N) ->
    ?q(begin
           E = ?s(G),
           ?s(hygenic_splice(G, ?r(E), N-1))
       end).

meta_var_test() ->
    E = 1,
    ?s(hygenic_splice(?r(E), ?q(42), 1)).

%%
%% "Long reference" example
%%
long_ref(A) ->
    ?s(?q(fun(B) ->
                  B + ?s(?r(A))
          end)).

long_ref_test() ->
    ?assertEqual(7, (long_ref(3))(4)).

%%
%% extract test
%%
simple_extract() ->
    Q = ?q(42),
    ?q(begin
           N = ?s(fun(QN) ->
                          ?v(QN)
                  end(?i(Q))),
           N
       end).

another_extract() ->
    Q = ?q(43),
    ?q(begin
           N = ?s(begin
                      QN = ?i(Q),
                      ?v(QN)
                  end),
           N
       end).

complex_extract() ->
    Q = ?q(42),
    ?q(?s(begin
              N = ?i(Q),
              Ns = erl_syntax:list([N,N,N]),
              L = erl_syntax:revert(Ns),
              ?v(L)
          end)).

hygienic_extract() ->
    Q1 = ?q(begin
               A = 21,
               _B = A * 2
           end),
    Q2 = ?q(begin
               A = 40,
               _B = A + 2
           end),
    ?q(?s(begin
              E1 = ?i(Q1),
              E2 = ?i(Q2),
              B = erl_syntax:block_expr([E1,E2]),
              Ast = erl_syntax:revert(B),
              ?v(Ast)
          end)).

simple_extract_test_() ->
     [?_assertEqual(42, ?s(simple_extract())),
      ?_assertEqual(43, ?s(another_extract())),
      ?_assertEqual([42,42,42], ?s(complex_extract())),
      ?_assertEqual(42, ?s(hygienic_extract()))].
    

%%
%% Utilities
%%
is_valid_quote(QExpr) ->
    try
        {Q,_} = QExpr(gb_sets:new()),
        erl_lint:exprs([Q], []),
        true
    catch error:_ ->
            false
    end.
