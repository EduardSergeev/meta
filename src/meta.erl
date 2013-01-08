%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%% Simple meta-programming for erlang
%%%
%%% Quasi-quotation-based, hygienic macros expanded in compile-time.
%%% Based on parse_transform  
%%% @end
%%% Created : Jun 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(meta).

-export([reify_type/2,
         reify_attributes/2,
         reify/2,
         error/2, error/3,

         parse_transform/2,
         format_error/1,

         hygienize_var/3]).

-include("../include/meta_syntax.hrl").

%%%
%%% Types
%%%
-type form() :: erl_parse:abstract_form().
-type forms() :: form() | [form()].
-type vars() :: gb_set().

-type quote(Form) :: fun((vars()) -> Form).

-record(info,
        {options = [],
         meta = [],
         attributes = dict:new(),
         imports = dict:new(),
         types = dict:new(),
         records = dict:new(),
         funs = dict:new(),
         vars = gb_sets:new()}).

-opaque info() :: #info{}.

-export_type([quote/1, info/0]).


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
-define(OP_CALL(Ln, Name, Arg1, Arg2),
        #op{line = Ln,
            name = Name,
            arg1 = Arg1,
            arg2 = Arg2}).
-define(META_CALL(Ln, Name, Args), ?REMOTE_CALL(Ln, meta, Name, Args)).

-define(QUOTE(Ln, Expr), ?META_CALL(Ln, quote, [Expr])).
-define(SPLICE(Ln, Expr), ?META_CALL(Ln, splice, [Expr])).
-define(REF(Ln, Var), ?META_CALL(Ln, ref, [Var])).
-define(VERBATIM(Ln, Expr), ?META_CALL(Ln, verbatim, [Expr])).
-define(EXTRACT(Ln, Expr), ?META_CALL(Ln, extract, [Expr])).

-define(REIFY(Ln, Name), ?META_CALL(Ln, reify, [Name])).
-define(REIFYTYPE(Ln, Name), ?META_CALL(Ln, reify_type, [Name])).
-define(REIFY_ALL_TYPES(Ln), ?META_CALL(Ln, reify_types, [])).
-define(REIFY_ALL(Ln), ?META_CALL(Ln, reify, [])).

-record(q_info,
        {info :: info(),
         level :: integer(),
         vars = dict:new(),
         refs :: dict(),
         splices = []}).

-record(s_info,
        {info :: info(),
         level :: integer(),
         refs :: dict(),
         splices = []}).


%%%
%%% API
%%%
-spec reify_type(TypeRef, Info) -> Def when
      TypeRef :: Record | Type | Fun,
      Record :: {record, RecordName},
      Type :: {TypeName, TypeArgs},
      TypeArgs :: [TypeRef],
      Fun :: {'fun', FunName, Arity},
      RecordName :: atom(),
      TypeName :: atom(),
      FunName :: atom(),
      Arity :: integer(),
      Info :: info(),
      Def :: form().
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
reify_type({tuple, Args}, Info) ->
    Args1 = [reify_type(A, Info) || A <- Args],
    {tuple, Args1};
reify_type({Name, Args}, #info{types = Ts}) ->
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

-spec reify_attributes(AttrName, Info) -> Form when
      AttrName :: atom(),
      Info :: info(),
      Form :: form().
reify_attributes(Name, #info{attributes = As}) ->
    lookup(Name, As, []).


reify({record,Name}, #info{records = Rs}) ->
    fetch(Name, Rs, {reify_unknown_record, Name}).

error(Module, Error) ->
    throw({external_error, Module, Error}).

error(Module, Error, Arg) ->
    throw({external_error, Module, {Error, Arg}}).


-spec hygienize_var(VarName, Vars, VarMaps) ->
                           {VarName, Vars, VarMaps} when
      VarName :: string(),
      Vars :: gb_set(),
      VarMaps :: dict().
hygienize_var('_', Vars, Maps) ->
    {'_', Vars, Maps};
hygienize_var(VarName, Vars, Maps) ->
    case dict:find(VarName, Maps) of
        {ok, NewVarName} ->
            {NewVarName, Vars, Maps};
        error ->
            {NewVarName, Vars1} =
                case gb_sets:is_member(VarName, Vars) of
                    true ->
                        New = escape(VarName, Vars),
                        {New, gb_sets:add(New, Vars)};
                    false ->
                        {VarName, gb_sets:add(VarName, Vars)}
                end,
            Maps1 = dict:store(VarName, NewVarName, Maps),
            {NewVarName, Vars1, Maps1}
    end.
            
    

-spec parse_transform(Forms, Options) -> Forms when
      Forms :: forms(),
      Options :: [compile:option()].
parse_transform(Forms, _Options) ->
    {Forms1, Info} = traverse(fun info/2, #info{}, Forms),
    Funs = [K || {K,_V} <- dict:to_list(Info#info.funs)],
    {_, Info1} = safe_mapfoldl(fun process_fun/2, Info, Funs),
    Forms2 = lists:map(insert(Info1), Forms1),
    dump_code(Forms2, Info1),
    Forms2.

-spec process_fun(Fun, Info) -> {Forms, Info} when
      Fun :: {FunName, Arity},
      FunName :: atom(),
      Arity :: integer(),
      Info :: info(),
      Forms :: forms().  
process_fun(Fun, #info{funs = Fs} = Info) ->
    {Def, State} = dict:fetch(Fun, Fs),
    if
        State =:= processed ->
            {Def, Info};
        State =:= raw ->
            {Def1, Info1} = expand_meta(Def, Info),
            {Def2, #info{funs = Fs1} = Info2} = meta(Def1, Info1),
            Fs2 = dict:store(Fun, {Def2, processed}, Fs1),
            {Def2, Info2#info{funs = Fs2}};
        true ->
            error({invalid_state, State})
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

expand_meta(?LOCAL_CALL(Ln, Name, Args) = Form, #info{meta = Ms} = Info) ->
    Fn = {Name, length(Args)},
    case lists:member(Fn, Ms) of
        true ->
            {Args1, Info1} = expand_meta(Args, Info),
            Args2 = [quote_arg(Ln, A) || A <- Args1],
            {?SPLICE(Ln, Form#call{args = Args2}), Info1};
        false ->
            traverse(fun expand_meta/2, Info, Form)
             end;
expand_meta(?OP_CALL(Ln, Name, Arg1, Arg2) = Form, #info{meta = Ms} = Info) ->
    Fn = {Name, 2},
    case lists:member(Fn, Ms) of
        true ->
            expand_meta(?LOCAL_CALL(Ln, Name, [Arg1, Arg2]), Info);
        false ->
            traverse(fun expand_meta/2, Info, Form)
    end;
expand_meta(Form, Info) ->
     traverse(fun expand_meta/2, Info, Form).

quote_arg(_, ?QUOTE(_, _) = Quote) ->
    Quote;
quote_arg(_, ?VERBATIM(_, _) = Verbatim) ->
    Verbatim;
quote_arg(_, ?REF(_, _) = Ref) ->
    Ref;
quote_arg(_, ?SPLICE(_, Expr)) ->
    Expr;
quote_arg(Ln, Arg) ->
    ?QUOTE(Ln, Arg).

-spec meta(Forms, Info) -> {Forms, Info} when
      Forms :: forms(),
      Info :: info().
meta(#function{} = Form, Info) ->
    Info1 = Info#info{vars = gb_sets:new()},
    traverse(fun meta/2, Info1, Form);
meta(#var{name = Name} = Form, #info{vars = Vs} = Info) ->
    Info1 = Info#info{vars = gb_sets:add(Name, Vs)},
    traverse(fun meta/2, Info1, Form);

meta(?QUOTE(_, Expr), Info) ->
    {Expr1, #q_info{info = Info1}} = process_quote(Expr, Info),
    {Expr1, Info1};

meta(?VERBATIM(_, Expr), Info) ->
    QI = #q_info
        {level = 1,
         info = Info,
         refs = dict:from_list(
                  [{V,0} || V <- gb_sets:to_list(Info#info.vars)])},
    {Expr1, #q_info{info = Info1}} = process_verbatim(Expr, QI),
    {Expr1, Info1};

meta(?REF(Ln, _), _) ->
    meta_error(Ln, standalone_ref);

meta(?SPLICE(Ln, Splice), Info) ->
    SI = #s_info
        {level = 0, info = Info,
         refs = dict:from_list(
                  [{V,0} || V <- gb_sets:to_list(Info#info.vars)])},
    {Splice1, SI1} = process_splice(Splice, SI),
    eval_splice(Ln, Splice1, SI1#s_info.info);   

meta(?REIFY(Ln, {'fun', _, {function, Name, Arity}}),
      #info{funs = Fs} = Info) ->
    Key = {Name, Arity},
    case dict:find(Key, Fs) of
        {ok, _} ->
            {Def, Info1} = process_fun(Key, Info),
            {Ast, #q_info{info = Info2}} = process_quote(Def, Info1),
            {Ast, Info2};
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
                    {Ast, #q_info{info = Info1}} = process_quote(Def, Info),
                    {Ast, Info1};
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


-spec process_quote(Forms, Info) -> {AST, QInfo} when
      Forms :: forms(),
      Info :: QInfo | info(),
      QInfo :: #q_info{},
      AST :: forms().      
process_quote(Quote, #info{} = Info) ->
    QI = #q_info
        {level = 1, info = Info,
         refs = dict:from_list(
                  [{V,0} || V <- gb_sets:to_list(Info#info.vars)])},
    process_quote(Quote, QI);    
process_quote(Quote, #q_info{} = QI) ->
    {Ast, QI1} = term_to_ast(Quote, QI),
    R = build_hygienizer(Ast, QI1),
    {R, QI1}.

-spec term_to_ast(Forms, QInfo) -> {AST, QInfo} when
      Forms :: forms(),
      QInfo :: #q_info{},
      AST :: erl_syntax:syntaxTree().
term_to_ast(#var{name = Name} = Var, QI) ->
    Vs = QI#q_info.vars,
    Rs = QI#q_info.refs,
    Level = QI#q_info.level,
    Vs1 = case dict:find(Name, Vs) of
              {ok, V} ->
                  Vs;
              error ->
                  Level = Level,
                  Index = dict:size(Vs),
                  VN = make_atom('Var', Level, Index),
                  V = erl_syntax:variable(VN),
                  dict:store(Name, V, Vs)
          end,
    Rs1 = dict:store(Name, Level, Rs),
    {Ast, _} = tuple_to_ast(Var, QI),
    {Ast1, _} = replace(Ast, {{tree,atom,{attr,0,[],none},Name}, V}),
    {Ast1, QI#q_info{vars = Vs1, refs = Rs1}};
term_to_ast(?QUOTE(Ln, _), _) ->
    meta_error(Ln, nested_quote);
term_to_ast(?SPLICE(_, Expr), QI) ->
    Level = QI#q_info.level,
    SI = #s_info
        {level = Level + 1,
         info = QI#q_info.info,
         refs = QI#q_info.refs,
         splices = QI#q_info.splices},
    {SExpr, SI1} = process_splice(Expr, SI),
    Ss = SI1#s_info.splices,
    SV = make_atom('S', Level, length(Ss)),
    QI1 = QI#q_info
        {splices = [{SV,SExpr} | Ss],
         info = SI1#s_info.info},
    {erl_syntax:variable(SV), QI1};
term_to_ast(?REF(Ln, _), _) ->
    meta_error(Ln, ref_in_quote);
term_to_ast(?VERBATIM(Ln, _), _) ->
    meta_error(Ln, verbatim_in_quote);
term_to_ast(Ls, QI) when is_list(Ls) ->
    {Ls1, QI1} = traverse(fun term_to_ast/2, QI, Ls),
    {erl_syntax:list(Ls1), QI1};
term_to_ast(T, QI) when is_tuple(T) ->
    tuple_to_ast(T, QI);
term_to_ast(I, QI) when is_integer(I) ->
    {erl_syntax:integer(I), QI};
term_to_ast(F, QI) when is_float(F) ->
    {erl_syntax:float(F), QI};
term_to_ast(A, QI) when is_atom(A) ->
    {erl_syntax:atom(A), QI}. 


tuple_to_ast(T, QI) ->
    Ls = case tuple_to_list(T) of
             [Tag,Pos|Xs] when is_integer(Pos) ->
                 [Tag,0|Xs];
             Tls ->
                 Tls
         end,
    {Ls1, QI1} = traverse(fun term_to_ast/2, QI, Ls),
    {erl_syntax:tuple(Ls1), QI1}.


replace(Form, {Form, NewForm} = Info) ->
    {NewForm, Info};
replace(Form, Info) ->
    traverse(fun replace/2, Info, Form).


build_hygienizer(Ast, QI) ->
    Vs = dict:to_list(QI#q_info.vars),
    Ss = QI#q_info.splices,
    Level = QI#q_info.level,
    {VEs, IV} = hygienize_vars(Vs, Level),
    {SEs, IS} = hygienize_splices(lists:reverse(Ss), Level, IV),
    EMaps0 = case VEs of
                 [] ->
                     [];
                 _ ->
                     QMaps0 = erl_syntax:variable(make_atom('_Maps', Level, 0)),
                     M = erl_syntax:atom(dict),
                     F = erl_syntax:atom(new),
                     QApp = erl_syntax:application(M, F, []),
                     [erl_syntax:match_expr(QMaps0, QApp)]
             end,
    QVars0 = erl_syntax:variable(make_atom('Vars', Level, 0)),
    QVarsN = erl_syntax:variable(make_atom('Vars', Level, IS)),
    QRes = erl_syntax:tuple([Ast, QVarsN]),
    Body = EMaps0 ++ VEs ++ SEs ++ [QRes],
    Cl = erl_syntax:clause([QVars0], none, Body), 
    E = erl_syntax:fun_expr([Cl]),
    erl_syntax:revert(E).

hygienize_vars(Vs, Level) ->
    Fun = fun({V,QV}, I) ->
                  QVars1 = erl_syntax:variable(make_atom('Vars', Level, I+1)),
                  QMaps1 = erl_syntax:variable(make_atom('_Maps', Level, I+1)),
                  QPat = erl_syntax:tuple([QV, QVars1, QMaps1]),
                  M = erl_syntax:atom(?MODULE),
                  F = erl_syntax:atom(hygienize_var),
                  QAV = erl_syntax:atom(V),
                  QVars = erl_syntax:variable(make_atom('Vars', Level, I)),
                  QMaps = erl_syntax:variable(make_atom('_Maps', Level, I)),
                  QArgs = [QAV, QVars, QMaps],
                  QApp = erl_syntax:application(M, F, QArgs),
                  Exp = erl_syntax:match_expr(QPat, QApp),
                  {Exp, I+1}
          end,
    lists:mapfoldl(Fun, 0, Vs).

hygienize_splices(Ss, Level, NI) ->
    Fun = fun({V,Expr}, I) ->
                  QV = erl_syntax:variable(V),
                  QVars1 = erl_syntax:variable(make_atom('Vars', Level, I+1)),
                  QPat = erl_syntax:tuple([QV, QVars1]),
                  QVars = erl_syntax:variable(make_atom('Vars', Level, I)),
                  QApp = erl_syntax:application(Expr, [QVars]),
                  Exp = erl_syntax:match_expr(QPat, QApp),
                  {Exp, I+1}
          end,
    lists:mapfoldl(Fun, NI, Ss).
    

-spec process_splice(Forms, SInfo) -> {AST, SInfo} when
      Forms :: forms(),
      SInfo :: #s_info{},
      AST :: erl_syntax:syntaxTree().
process_splice(Expr, SI) ->
    {Ast, SI1} = ast_to_ast(Expr, SI),
    Ast1 = erl_syntax:revert(Ast),
    {Ast1, SI1}.

-spec ast_to_ast(Forms, SInfo) -> {AST, SInfo} when
      Forms :: forms(),
      SInfo :: #s_info{},
      AST :: erl_syntax:syntaxTree().
ast_to_ast(?SPLICE(Ln, _), _) ->
    meta_error(Ln, nested_splice);
ast_to_ast(?VERBATIM(_, Expr), SI) ->
    QI = #q_info
        {level = SI#s_info.level + 1,
         info = SI#s_info.info,
         refs = SI#s_info.refs},
    {Ast, QI1} = process_verbatim(Expr, QI),
    {Ast, SI#s_info{info = QI1#q_info.info}};
ast_to_ast(?QUOTE(_Ln, Form), SI) ->
    QI = #q_info
        {level = SI#s_info.level + 1,
         info = SI#s_info.info,
         refs = SI#s_info.refs},
    {Ast, QI1} = process_quote(Form, QI),
    {Ast, SI#s_info{info = QI1#q_info.info}};
ast_to_ast(?REF(Ln, #var{name = Name} = Var), SI) ->
    Vs = SI#s_info.refs,
    Level = SI#s_info.level,
    case dict:find(Name, Vs) of
        {ok, 0} ->
            {QVar, _} = tuple_to_ast(Var, #q_info{level = 0}),
            QFunVar = erl_syntax:variable(make_atom('Vars', 1, 0)),
            Body = [erl_syntax:tuple([QVar, QFunVar])],
            Cl = erl_syntax:clause([QFunVar], none, Body), 
            E = erl_syntax:fun_expr([Cl]),
            {erl_syntax:revert(E), SI};
        {ok, VarLevel} ->
            QVar = erl_syntax:variable(make_atom('Var', Level+1, 0)),
            QVars1 = erl_syntax:underscore(),
            QMaps1 = erl_syntax:underscore(),
            QPat = erl_syntax:tuple([QVar, QVars1, QMaps1]),
            M = erl_syntax:atom(?MODULE),
            F = erl_syntax:atom(hygienize_var),
            QAV = erl_syntax:atom(Name),
            QVars = erl_syntax:variable(make_atom('Vars', VarLevel, 0)),
            QMaps = erl_syntax:variable(make_atom('_Maps', VarLevel, 0)),
            QArgs = [QAV, QVars, QMaps],
            QApp = erl_syntax:application(M, F, QArgs),
            Expr = erl_syntax:match_expr(QPat, QApp),
            QFunVar = erl_syntax:variable(make_atom('Vars', Level+1, 0)),
            
            {Ast, _} = tuple_to_ast(Var, #q_info{level = VarLevel}),
            {Ast1, _} = replace(Ast, {{tree,atom,{attr,0,[],none},Name}, QVar}),

            Res = erl_syntax:tuple([Ast1, QFunVar]),
            Body = [Expr, Res],
            Cl = erl_syntax:clause([QFunVar], none, Body), 
            E = erl_syntax:fun_expr([Cl]),
            {erl_syntax:revert(E), SI};
        error ->
            meta_error(Ln, {unknow_ref, Name})
    end;
ast_to_ast(?EXTRACT(_, Expr), SI) ->
    {SExpr, SI1} = process_splice(Expr, SI),
    Ss = SI1#s_info.splices,
    Level = SI1#s_info.level,
    SV = make_atom('S', Level-1, length(Ss)),
    SI2 = SI1#s_info
        {splices = Ss ++ [{SV,SExpr}]},
    {erl_syntax:revert(erl_syntax:variable(SV)), SI2};

ast_to_ast(?REIFY_ALL(_Ln), SI) ->
    {erl_syntax:revert(erl_parse:abstract(SI#s_info.info)), SI};
ast_to_ast(Form, SI) ->
    traverse(fun ast_to_ast/2, SI, Form).



process_verbatim(Verbatim, QI) ->
    {Ast, QI1} = verbatim_to_ast(Verbatim, QI),
    R = build_hygienizer(Ast, QI1),
    {R, QI1}.

verbatim_to_ast(?QUOTE(Ln, _), _) ->
    meta_error(Ln, quote_in_verbatim);
verbatim_to_ast(?SPLICE(_, Expr), QI) ->
    Level = QI#q_info.level,
    SI = #s_info
        {level = Level + 1,
         info = QI#q_info.info,
         refs = QI#q_info.refs},
    {SExpr, SI1} = process_splice(Expr, SI),
    Ss = QI#q_info.splices,
    SV = make_atom('S', Level, length(Ss)),
    QI1 = QI#q_info
        {splices = [{SV,SExpr} | Ss],
         info = SI1#s_info.info},
    {erl_syntax:revert(erl_syntax:variable(SV)), QI1};
verbatim_to_ast(?REIFY_ALL(_Ln), QI) ->
    {erl_parse:abstract(QI#q_info.info), QI};
verbatim_to_ast(Form, QI) ->
    traverse(fun verbatim_to_ast/2, QI, Form).
    

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
            {Ast, #q_info{info = Info1}} = process_quote(Def, Info),
            {Ast, Info1};
        error ->
            meta_error(Line, Error)
    end.

lookup(Name, Dict, Default, Info) ->
    case dict:find(Name, Dict) of
        {ok, Def} ->
            {Ast, #q_info{info = Info1}} =  process_quote(Def, Info),
            {Ast, Info1};
        error ->
            Default
    end.


%%
%% Various info gathering for subsequent use
%%
-spec info(Forms, Info) -> {Forms, Info} when
      Forms :: forms(),
      Info :: info().
info(#attribute{name = meta, arg = Meta} = Form,
     #info{meta = Ms} = Info) ->
    Info1 = Info#info{meta = Ms ++ Meta},
    {Form, Info1};
info(#attribute{name = meta_opts, arg = Opts} = Form,
     #info{options = Os} = Info) ->
    Os1 = Os ++ if is_list(Opts) -> Opts ; true -> [Opts] end,
    Info1 = Info#info{options = Os1},
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
    

eval_splice(Ln, Splice, #info{vars = Vs} = Info) ->
    Bs = erl_eval:new_bindings(),
    Local = {eval, local_handler(Ln, Info)},
    try
        {value, Fun, _} = erl_eval:expr(Splice, Bs, Local),
        {Expr, _} = Fun(Vs),
        {Expr1, _} = set_pos(Expr, Ln),
        erl_lint:exprs([Expr1], []),
        {Expr1, Info}
    catch
        throw:{get_line, MetaError} ->
            meta_error(Ln, MetaError);
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
            [{Mod, Fn, Args, _}|_] = erlang:get_stacktrace(),
            meta_error(Ln, {splice_unknown_external_function, {Mod, Fn, length(Args)}});
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
                            Local = {eval, local_handler(Ln, Info1)},
                            {Bs1, Args1} = hygienize(Bs, Args, Cs),

                            F = erl_syntax:fun_expr(Cs),
                            A = erl_syntax:application(F, Args1),
                            Call = erl_syntax:revert(A),
                            TVal = erl_eval:expr(Call, Bs1, Local),
                            setelement(3, TVal, Bs);
                        false ->
                            meta_error(Ln, {splice_unknown_function, Fn})
                    end
            end
    end.  

collect_vars(#var{name = Name} = Form, Set) ->
    {Form, gb_sets:add(Name, Set)};
collect_vars(Form, Set) ->
    traverse(fun collect_vars/2, Set, Form).

hygienize(Bs, Args, Cs) ->
    {_, CsNs} = collect_vars(Cs, gb_sets:new()),
    SBs = gb_sets:from_list([ N || {N,_} <- erl_eval:bindings(Bs) ]),
    Mss = {SBs, dict:new(), CsNs},
    {Args1, {_, BMs1, _}} = lists:mapfoldl(
                             fun hygienize_arg/2,
                             Mss, Args),
    Bs1 = filter_replace_bs(Bs, BMs1),
    {Bs1, Args1}.

hygienize_arg(Arg, {Bs, BMs, CsNs}) ->
    TMs = dict:new(),
    Mss = {Bs, BMs, TMs, CsNs},
    {Arg1, {Bs, BMs1, _, CsNs1}} = replace_vars(Arg, Mss),
    {Arg1, {Bs, BMs1, CsNs1}}.

hyg_var(Name, {Bs, BMs, TMs, CsNs}) ->
    case gb_sets:is_member(Name, Bs) of
        true ->
            case dict:find(Name, BMs) of
                {ok, NewName} ->
                    {NewName, {Bs, BMs, TMs, CsNs}};
                error ->
                    NewName = escape(Name, CsNs),
                    BMs1 = dict:store(Name, NewName, BMs),
                    CsNs1 = gb_sets:add(NewName, CsNs),
                    {NewName, {Bs, BMs1, TMs, CsNs1}}
            end;
        false ->
            case dict:find(Name, TMs) of
                {ok, NewName} ->
                    {NewName, {Bs, BMs, TMs, CsNs}};
                error ->
                    NewName = escape(Name, CsNs),
                    TMs1 = dict:store(Name, NewName, TMs),
                    CsNs1 = gb_sets:add(NewName, CsNs),
                    {NewName, {Bs, BMs, TMs1, CsNs1}}
            end
    end.
                
replace_vars({var, _, '_'} = Var, Mss) ->
    {Var, Mss};
replace_vars({var, Ln, N}, Mss) ->
    {N1, Mss1} = hyg_var(N, Mss),
    {{var, Ln, N1}, Mss1};
replace_vars(Form, Mss) ->
    traverse(fun replace_vars/2, Mss, Form).

filter_replace_bs(Bs, BMs) ->
    NVMs = [ {dict:find(N, BMs), V} || {N, V} <- erl_eval:bindings(Bs) ],
    NVs = [ {N1,V} || {{ok, N1}, V} <- NVMs ],
    lists:foldl(
      fun({N1,V}, Bs1) ->
              erl_eval:add_binding(N1,V,Bs1)
      end, erl_eval:new_bindings(), NVs).
    
escape(N, CsNs) ->
    case gb_sets:is_member(N, CsNs) of
        false ->
            N;
        true ->
            escape_iter(N, 1, CsNs)
    end.

escape_iter(N, I, CsNs) ->
    SN1 = lists:flatten(format("~s~B", [N,I])),
    N1 = list_to_atom(SN1),
    case gb_sets:is_member(N1, CsNs) of
        false ->
            N1;
        true ->
            escape_iter(N, I+1, CsNs)
    end.

                
%%
%% Type reification functions
%%
is_standard({Type,0}) ->
    Ts = [integer, float, binary, boolean, atom,
          byte, char, number, string, any, tuple],
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
%% Compile-time error
%%
meta_error(Line, Error) ->
    throw({Line, Error}).

meta_error(Line, Error, Arg) ->
    throw({Line, {Error, Arg}}).

external_error(Line, Module, Error) ->
    throw({Line, Module, Error}).

%%
%% Dumping function
%%
dump_code(Forms, #info{options = Opts}) ->
    case lists:member(dump_code, Opts) of
        true ->
            io:format(
              "~s~n",
              [erl_prettypr:format(erl_syntax:form_list(Forms))]);
        false ->
            ok
    end.


%%
%% Formats error messages for compiler 
%%
-spec format_error(any()) -> iolist().
format_error(nested_quote) ->
    "meta:quote/1 is not allowed within another meta:quote/1";
format_error(nested_splice) ->
    "meta:splice/1 is not allowed within another meta:splice/1";
format_error({nested_meta_function, {Name, Arity}}) ->
    format("-meta function '~s/~b' is not allowed within meta:splice/1",
           [Name, Arity]);                    
format_error({nested_meta_op, Name}) ->
    format("-meta operator '~s' is not allowed within meta:splice/1",
           [Name]);                    
format_error({reify_unknown_function, {Name, Arity}}) ->
    format("attempt to reify unknown function '~s/~b'",
           [Name, Arity]);
format_error({reify_unknown_record, Name}) ->
    format("attempt to reify unknown record '#~s{}'", [Name]);
format_error({reify_unknown_type, Type}) ->
    format("attempt to reify unknown type '~s'", [type_to_list(Type)]);
format_error({reify_unknown_record_type, Name}) ->
    format("attempt to reify unknown record type '#~s{}'", [Name]);
format_error({reify_unknown_function_spec, {Name, Arity}}) ->
    format("attempt to reify unknown function -spec '~s/~b'",
           [Name, Arity]);
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
format_error({splice_unknown_external_function, {Mod, Fn, Arity}}) ->
    format("Unknown function '~s:~s/~b' used in 'meta:splice/1'",
           [Mod, Fn, Arity]);    
format_error({splice_unknown_function, {Name,Arity}}) ->
    format("Unknown local function '~s/~b' used in 'meta:splice/1'",
           [Name,Arity]);
format_error({unknown_ref, Name}) ->
    format("Reference to unbound variable: '~s'", [Name]);
format_error(standalone_ref) ->
    "Reference is valid only within meta:splice/1";
format_error(ref_in_quote) ->
    "Reference is valid only within meta:splice/1";
format_error(standalone_verbatim) ->
    "Verbatim is valid only within meta:quote/1";
format_error(verbatim_in_quote) ->
    "meta:verbatin/1 is not allowed within meta:quote/1";
format_error(quote_in_verbatim) ->
    "meta:quote/1 is not allowed within meta:verbatim/1".

type_to_list({Name, Args}) ->
    Args1 = string:join([type_to_list(T) || T <- Args], ","),
    lists:flatten(format("~s(~s)", [Name, Args1])).




%%
%% Utils
%%    
format(Format, Args) ->
    io_lib:format(Format, Args).

make_atom(Base, Level, Index) ->
    List = lists:flatten(format("~s_~B_~B", [Base, Level, Index])),
    list_to_atom(List).
    
