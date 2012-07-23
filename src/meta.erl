-module(meta).

-export([reify_type/2,
         reify_attributes/2,
         reify/2,
         error/2, error/3]).

-export([parse_transform/2,
         format_error/1]).

-include("../include/meta_syntax.hrl").

-record(info,
        {meta = [],
         attributes = dict:new(),
         imports = dict:new(),
         types = dict:new(),
         records = dict:new(),
         funs = dict:new()}).

-define(REMOTE_CALL(Ln, Mod, Name, Args),
        #call{line = Ln,
              function = #remote
              {module = #atom{name = Mod},
               name = #atom{name = Name}},
              args = Args}).
-define(LOCAL_CALL(Ln, Name, Args),
        #call{line = Ln,
              function = #atom{name = Name},
              args = Args}).
-define(META_CALL(Ln, Name, Args), ?REMOTE_CALL(Ln, meta, Name, Args)).
-define(LN(Ln), ?META_CALL(Ln, line, [])).
-define(QUOTE(Ln, Expr), ?META_CALL(Ln, quote, [Expr])).

-define(REIFY(Ln, Name), ?META_CALL(Ln, reify, [Name])).
-define(REIFYTYPE(Ln, Name), ?META_CALL(Ln, reify_type, [Name])).
-define(REIFY_ALL_TYPES(Ln), ?META_CALL(Ln, reify_types, [])).
-define(REIFY_ALL(Ln), ?META_CALL(Ln, reify, [])).

-define(SPLICE(Ln, Expr), ?META_CALL(Ln, splice, [Expr])).


%%%
%%% API
%%%
reify_type({record,_Name} = Record, #info{types = Ts} = Info) ->
    Key = {Record,0},
    case lookup(Key, Ts, none) of
        none ->
            {_,Fields} = reify(Record, Info),
            {Record,Fields,[]};
        Def ->
            Def
    end;    
reify_type({'fun',Name,Arity} = Fun, #info{types = Ts}) ->
    fetch(Fun, Ts, {reify_unknown_function_spec, {Name, Arity}});
reify_type({Name,Args}, #info{types = Ts}) ->
    Key = {Name,length(Args)},
    case lookup(Key, Ts, none) of
        none ->
            case is_standard(Key) of
                true ->
                    Key;
                false ->
                    meta_error(get_line, {reify_unknown_type, {Name,Args}})
            end;
        Def ->
            Def
    end.


reify_attributes(Name, #info{attributes = As}) ->
    lookup(Name, As, []).


reify({record,Name}, #info{records = Rs}) ->
    fetch(Name, Rs, {reify_unknown_record, Name}).

error(Module, Error) ->
    throw({external_error, Module, Error}).

error(Module, Error, Arg) ->
    throw({external_error, Module, {Error, Arg}}).

    

parse_transform(Forms, _Options) ->
    %%io:format("~p", [Forms]),
    {Forms1, Info} = traverse(fun info/2, #info{}, Forms),
    %%io:format("~p", [Info]),
    Funs = [K || {K,_V} <- dict:to_list(Info#info.funs)],
    {_, Info1} = safe_mapfoldl(fun process_fun/2, Info, Funs),
    %% io:format("~p", [Info1]),
    Forms2 = lists:map(insert(Info1), Forms1),
    %%io:format("~p", [Forms2]),
    %%io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms2))]),
    Forms2.


%%
%% meta:quote/1 handling
%%
process_fun(Fun, #info{funs = Fs} = Info) ->
    {Def, State} = dict:fetch(Fun, Fs),
    if
        State =:= processed ->
            {Def, Info};
        State =:= raw ->
            {Def1, #info{funs = Fs1} = Info1} = meta(Def, Info),
            Fs2 = dict:store(Fun, {Def1, processed}, Fs1),
            {Def1, Info1#info{funs = Fs2}}
    end.

insert(#info{funs = Fs}) ->
    fun(#function{name = Name, arity = Arity}) ->
            Fun = {Name, Arity},
            case dict:fetch(Fun, Fs) of
                {Def, processed} ->
                    Def;
                {error, _} = Err ->
                    Err
            end;
       (Form) ->
            Form
    end.

meta(?LOCAL_CALL(Ln, Name, Args) = Form, #info{meta = Ms} = Info) ->
    Fn = {Name, length(Args)},
    case lists:member(Fn, Ms) of
        true ->
            QFun = fun(Arg, Info1) -> meta(?QUOTE(Ln, Arg), Info1) end,
            {Args1, Info2} = traverse(QFun, Info, Args),
            eval_splice(Ln, ?LOCAL_CALL(Ln, Name, Args1), Info2);
        false ->
            traverse(fun meta/2, Info, Form)
    end;
meta(?QUOTE(_, Quote), Info) ->
    {Ast, Info1} = term_to_ast(Quote, Info),
    {erl_syntax:revert(Ast), Info1};
meta(?SPLICE(Ln, Splice), Info) ->
    {Splice1, Info1} = process_splice(Splice, Info),
    eval_splice(Ln, Splice1, Info1);

meta(?REIFY(Ln, {'fun', _, {function, Name, Arity}}),
      #info{funs = Fs} = Info) ->
    Key = {Name, Arity},
    case dict:find(Key, Fs) of
        {ok, _} ->
            {Def, Info1} = process_fun(Key, Info),
            {Ast, Info2} = term_to_ast(Def, Info1),
            {erl_syntax:revert(Ast), Info2};
        error ->
            meta_error(Ln, {reify_unknown_function, Key})
    end;
meta(?REIFY(Ln, {record, _, Name, []}),
      #info{records = Rs} = Info) ->
    fetch(Ln, Name, Rs, {reify_unknown_record, Name}, Info);             
meta(?REIFYTYPE(Ln, #call{function = #atom{name = Name}, args = Args}),
      #info{types = Ts} = Info) ->
    Key = {Name, length(Args)},
    fetch(Ln, Key, Ts, {reify_unknown_type, {Name, Args}}, Info);             
meta(?REIFYTYPE(Ln, {record, _, Name, []}),
      #info{types = Ts, records = Rs} = Info) ->
    Key = {Rec = {record, Name}, 0},
    case lookup(Key, Ts, none, Info) of
        none ->
            case dict:find(Name, Rs) of
                {ok, {_,Fields}} ->
                    Def = {Rec,Fields,[]},
                    {Ast, Info1} = term_to_ast(Def, Info),
                    {erl_syntax:revert(Ast), Info1};
                error ->
                    meta_error(Ln, {reify_unknown_record, Name})
            end;
        Result ->
            Result
    end;    
meta(?REIFYTYPE(Ln, {'fun', _, {function, Name, Arity}}),
      #info{types = Ts} = Info) ->
    Key = {'fun', Name, Arity},
    fetch(Ln, Key, Ts, {reify_unknown_function_spec, Key}, Info);
meta(?REIFY_ALL_TYPES(_Ln), Info) ->
    {erl_parse:abstract(Info#info.types), Info};
meta(?REIFY_ALL(_Ln), Info) ->
    {erl_parse:abstract(Info), Info};

meta(#attribute{} = Form, Info) ->
    {Form, Info};

meta(Form, Info) ->
    traverse(fun meta/2, Info, Form).


term_to_ast(?QUOTE(Ln, _), _) ->
    meta_error(Ln, nested_quote);
term_to_ast(?SPLICE(_Ln, Form), Info) ->
    meta(Form, Info);
term_to_ast(Ls, Info) when is_list(Ls) ->
    {Ls1, _} = traverse(fun term_to_ast/2, Info, Ls),
    {erl_syntax:list(Ls1), Info};
term_to_ast(T, Info) when is_tuple(T) ->
    Ls = case tuple_to_list(T) of
             [Tag,Pos|Xs] when is_integer(Pos) ->
                 [Tag,0|Xs];
             Tls ->
                 Tls
         end,
    {Ls1, Info1} = traverse(fun term_to_ast/2, Info, Ls),
    {erl_syntax:tuple(Ls1), Info1};
term_to_ast(I, Info) when is_integer(I) ->
    {erl_syntax:integer(I), Info};
term_to_ast(F, Info) when is_float(F) ->
    {erl_syntax:float(F), Info};
term_to_ast(A, Info) when is_atom(A) ->
    {erl_syntax:atom(A), Info}.    


process_splice(?SPLICE(Ln, _), _) ->
    meta_error(Ln, nested_splice);
process_splice(?QUOTE(_Ln, _Form) = Q, Info) ->
    meta(Q, Info);
process_splice(Form, Info) ->
    traverse(fun process_splice/2, Info, Form).


fetch(Name, Dict, Error) ->
    case dict:find(Name, Dict) of
        {ok, Def} ->
            Def;
        error ->
            meta_error(get_line, Error)
    end.

lookup(Name, Dict, Default) ->
    case dict:find(Name, Dict) of
        {ok, Def} ->
            Def;
        error ->
            Default
    end.


fetch(Line, Name, Dict, Error, Info) ->
    case dict:find(Name, Dict) of
        {ok, Def} ->
            {Ast, Info1} = term_to_ast(Def, Info),
            {erl_syntax:revert(Ast), Info1};
        error ->
            meta_error(Line, Error)
    end.

lookup(Name, Dict, Default, Info) ->
    case dict:find(Name, Dict) of
        {ok, Def} ->
            {Ast, Info1} = term_to_ast(Def, Info),
            {erl_syntax:revert(Ast), Info1};
        error ->
            Default
    end.


%%
%% Various info gathering for subsequent use
%%
info(#attribute{name = meta, arg = Meta} = Form,
     #info{meta = Ms} = Info) ->
    Info1 = Info#info{meta = Ms ++ Meta},
    {Form, Info1};
info(#attribute{name = import, arg = {Mod, Fs}} = Form,
     #info{imports = Is} = Info) ->
    Is1 = lists:foldl(fun(F,D) -> dict:store(F, {Mod,F}, D) end, Is, Fs),
    Info1 = Info#info{imports = Is1},
    {Form, Info1};
info(#attribute{name = record, arg = {Name, _} = Def} = Form,
     #info{records = Rs} = Info) ->
    Rs1 = dict:store(Name, Def, Rs),
    Info1 = Info#info{records = Rs1},
    {Form, Info1};
info(#attribute{name = type, arg = Def} = Form,
     #info{types = Ts} = Info) ->
    {Type, _, Args} = Def,
    Key = {Type, length(Args)},
    Ts1 = dict:store(Key, Def, Ts),
    Info1 = Info#info{types = Ts1},
    {Form, Info1};
info(#attribute{name = spec, arg = Def} = Form,
     #info{types = Ts} = Info) ->
    {Name, Arity} = element(1, Def),
    Key = {'fun', Name, Arity},
    Ts1 = dict:store(Key, Def, Ts),
    Info1 = Info#info{types = Ts1},
    {Form, Info1};
info(#attribute{name = Name, arg = Arg} = Form,
     #info{attributes = As} = Info) ->
    As1 = dict:append(Name, Arg, As),
    Info1 = Info#info{attributes = As1},
    {Form, Info1};
info(#function{name = Name, arity = Arity} = Form,
     #info{funs = Fs} = Info) ->
    Key = {Name,Arity},
    Value = {Form, raw},
    Info1 = Info#info{funs = dict:store(Key, Value, Fs)},
    Form1 = Form#function{clauses = undefined},
    {Form1, Info1};
info(Form, Info) ->
    traverse(fun info/2, Info, Form).
    

eval_splice(Ln, Splice, Info) ->
    Bs = erl_eval:new_bindings(),
    Local = {eval, local_handler(Ln, Info)},
    try
        {value, Val, _} = erl_eval:expr(Splice, Bs, Local),
        Expr = erl_syntax:revert(Val),
        {Expr1, _} = set_pos(Expr, Ln),
        erl_lint:exprs([Expr1], []),
        {Expr1, Info}
    catch
        throw:{external_error, Module, Error} ->
            external_error(Ln, Module, Error);
        error:{unbound, Var} ->
            meta_error(Ln, splice_external_var, Var);
        error:{badarity, _} ->
            meta_error(Ln, splice_badarity);
        error:{badfun, _} ->
            meta_error(Ln, splice_badfun);
        error:{badarg, Arg} ->
            meta_error(Ln, splice_badarg, Arg);
        error:undef ->
            meta_error(Ln, splice_unknown_external_function);
        error:_ ->
            meta_error(Ln, invalid_splice)
    end.

local_handler(Ln, Info) ->
    fun(Name, Args, Bs) ->
            #info{imports = Is, funs = Fs} = Info,
            Fn = {Name, length(Args)},
            case dict:find(Fn, Is) of
                {ok, {Mod,{Fun,_}}} ->
                    M = erl_syntax:atom(Mod),
                    F = erl_syntax:atom(Fun),
                    A = erl_syntax:application(M, F, Args),
                    Call = erl_syntax:revert(A),
                    Local = {eval, local_handler(Ln, Info)},
                    erl_eval:expr(Call, Bs, Local);      
                error ->
                    case dict:is_key(Fn, Fs) of
                        true ->
                            {#function{clauses = Cs}, #info{} = Info1} = process_fun(Fn, Info),
                            F = erl_syntax:fun_expr(Cs),
                            A = erl_syntax:application(F, Args),
                            Call = erl_syntax:revert(A),
                            Local = {eval, local_handler(Ln, Info1)},
                            erl_eval:expr(Call, Bs, Local);
                        false ->
                            meta_error(Ln, {splice_unknown_function, Fn})
                    end
            end
    end.  

%%
%% Type reification functions
%%
is_standard({Type,0}) ->
    Ts = [integer, float, binary, boolean, atom, tuple,
          byte, char, number, string, any],
    lists:member(Type, Ts);
is_standard(_) ->
    false.

%%
%% Recursively set position (Line number) to Pos
%%
set_pos(Tuple, Pos) when
      is_tuple(Tuple) andalso 
      is_integer(element(2, Tuple)) ->
    Tuple1 = setelement(2, Tuple, Pos),
    traverse(fun set_pos/2, Pos, Tuple1);
set_pos(Form, Pos) ->
    traverse(fun set_pos/2, Pos, Form).


    

%%
%% Recursive traversal a-la mapfoldl
%%
traverse(Fun, Acc, Form) when is_tuple(Form) ->
    Fs = tuple_to_list(Form),
    {Fs1, Acc1} = traverse(Fun, Acc, Fs),
    {list_to_tuple(Fs1), Acc1};
traverse(Fun, Acc, Fs) when is_list(Fs) ->
    lists:mapfoldl(Fun, Acc, Fs);
traverse(_Fun, Acc, Smt) ->
    {Smt, Acc}.

safe_mapfoldl(Fun, Info, Fns) ->
    Do = fun(Fn, #info{funs = Fs} = I) ->
                 try
                     Fun(Fn, I)
                 catch
                     throw:{Line, Reason} ->
                         E = {error, {Line, ?MODULE, Reason}},
                         {E, I#info{funs = dict:store(Fn, E, Fs)}};
                     throw:{Line, Module, Reason} ->
                         E = {error, {Line, Module, Reason}},
                         {E, I#info{funs = dict:store(Fn, E, Fs)}}
                 end
         end,    
    lists:mapfoldl(Do, Info, Fns).

%%
%% Compile-time errorr
%%
meta_error(Line, Error) ->
    throw({Line, Error}).

meta_error(Line, Error, Arg) ->
    throw({Line, {Error, Arg}}).

external_error(Line, Module, Error) ->
    throw({Line, Module, Error}).



%%
%% Formats error messages for compiler 
%%
format_error(nested_quote) ->
    "meta:quote/1 is not allowed within another meta:quote/1";
format_error(nested_splice) ->
    "meta:splice/1 is not allowed within another meta:splice/1";
format_error({reify_unknown_function, {Name, Arity}}) ->
    format("attempt to reify unknown function '~s/~b'", [Name, Arity]);
format_error({reify_unknown_record, Name}) ->
    format("attempt to reify unknown record '#~s{}'", [Name]);
format_error({reify_unknown_type, {Name,Args}}) ->
    Args1 = string:join([format("~p", A) || A <- Args], ","),
    format("attempt to reify unknown type '~s(~s)'", [Name, Args1]);
format_error({reify_unknown_record_type, Name}) ->
    format("attempt to reify unknown record type '#~s{}'", [Name]);
format_error({reify_unknown_function_spec, {Name, Arity}}) ->
    format("attempt to reify unknown function -spec '~s/~b'", [Name, Arity]);
format_error({reify_unknown_attribute, Name}) ->
    format("attempt to reify unknown attribute '~s'", [Name]);
format_error(invalid_splice) ->
    "invalid expression in meta:splice/1";
format_error({splice_external_var, Var}) ->
    format("Variable '~s' is outside of scope of meta:splice/1", [Var]);
format_error(splice_badarity) ->
    "'badarity' call in 'meta:splice'";
format_error(splice_badfun) ->
    "'badfun' call in 'meta:splice'";
format_error({splice_badarg, Arg}) ->
    format("'badarg' in 'meta:splice': ~p", [Arg]);
format_error(splice_unknown_external_function) ->
    "Unknown remote function call in 'splice'";
format_error({splice_unknown_function, {Name,Arity}}) ->
    format("Unknown local function '~s/~b' used in 'meta:splice/1'", [Name,Arity]).
    
format(Format, Args) ->
    io_lib:format(Format, Args).
