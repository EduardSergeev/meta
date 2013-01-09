Meta
=======

Overview
--------
Meta is an attempt to simplify meta-programming for Erlang provided by [parse_transform/2](http://www.erlang.org/doc/man/erl_id_trans.html). The library is enspired by [Template Haskell](http://www.haskell.org/haskellwiki/Template_Haskell) especially by its quasi-quotation mechanism.
As a result, using meta it is possible to inspect and generate Erlang code in compile-time often without the need to deal with [internal Erlang AST](http://erlang.org/doc/man/erl_syntax.html).

How does it work
----------------
Meta offers several primitive "meta-function" (`meta:quote/1` and `meta:splice/1` being the most commonly used) which, when being inserted in Erlang code, mark the areas of code which should be pre-processed by `meta` library in compile-time. Upon compilation such places are transformed into Erlang code fragments using normal Erlang functions and expressions (which in this case are evaluated in compile-time). Even though it is possible to manipulate Erlang AST directly, quote/splice mechanism can greately simplify such manipulations in many cases. 


Quickstart examples
-------------------
To use `meta` simply add the following header:
 
    -include_lib("meta/include/meta.hrl").

Then, to turn any piece of Erlang code into "quote" wrap it into `meta:quote/1` which can be conveniently abbreviated with `?q/1` macro:

    get_quote() ->
        ?q(1 + 2).

The above function when called returns not the number `3` but a "quote" of Erlang expression `1+2`. Alone this qunction is mostly useless (well, you can probably look at the resulting AST using `?e/1` macro) but the realy usefull thing whech we can do with it is to "splice" (insert) our quote into another Erlang code using `meta:splice/1` expression (abbreviated with `?s/1` macro):

    use_quote() ->
        ?s(get_quote()) + 39.

The body of the above function is produced in compile type by calling local function `get_quote/0` and inserting the returned quote `1+2` before `+ 39` which results in:

    use_quote() ->
        1 + 2 + 39.

Note: it is possible to check the result of splice using attribute `-meta_opts(dump_code)`. Whenever this attribute is added to module code the entire module code (after macro-expansion) will be printed during the compilation (warning:the otput can be quite large).

When called `use_quote/0` returns `42` which is understandable but not very exiting. Of course the above type of quote/splice is useless - the same (well, almost) result could be achieved with a normal function call without any meta-programming. However this example illustrates a crusial difference of meta-function: the function `get_quote/0` is evaluated in compile-time only and whatever Erlang code is produced it then becomes a part of the resulting code and can be executed in run-time. The logic of such meta-function can be quite complex and it can produce the code which, for instance, otherwise would have to be written by hand.
Still it is not very useful example, lets do something more meta-like and to do that we should note that quotes and splices can be _nested_:

    get_nested_quote(Quote) ->
        ?q(1 + ?s(Quote)).

The above function returns a quote which is produced by splicing passed in `Quote` quote into internal quote. Here is how we can splice this quote:

    use_nested_quote() ->
        ?s(get_nested_quote(?q(2 + 3))) + 37.
    
Note that if we try to pass a quote of some incompatible type, like `atom()`, we get compile-time warning since the Erlang compiler can see that the resulting code is wrong:

    %% This produces "Warning: this expression will fail with a 'badarith' exception"
    create_warning() ->
        ?s(get_nested_quote(?q(undefined))) + 37.

The warning is generated because the resulting function has the following body:

    create_warning() ->
        1 + undefined + 37.

Now lets do something more compelling: can we, for example, inline (unwind) a loop in code? Say, we need to apply some simple function like

    inc(Arg) ->
        Arg + 1.

several times to a passed argument. We can do this either via direct recursion:

    triple_inc1(Arg) ->
        app_iter(3, Arg).
        
    app_iter(0, Arg) ->
        Arg;
    app_iter(N, Arg) ->
       app_iter(N-1, inc(Arg)).

or via higher order function-combinator:

    triple_inc2(Arg) ->
        lists:foldl(
          fun(_, A) -> inc(A) end,
          Arg, lists:seq(1,3)).
   
It turns out that both approaches can be used to *generate* a code which simply encodes triple `inc/1` application:

    triple_inc3(Arg) ->
        ?s(q_iter(3, ?r(Arg))).
        
    q_iter(0, QArg) ->
        QArg;
    q_iter(N, QArg) ->
       ?q(inc(?s(q_iter(N-1, QArg)))).

and

    triple_inc4(Arg) ->
        ?s(lists:foldl(
             fun(_, Q) -> ?q(inc(?s(Q))) end,
             ?r(Arg), lists:seq(1,3))).

As a result both `trice3/1` and `trice4/1` contains identical code:

    triple_inc3_4(Arg) ->
        inc(inc(inc(Arg))).

Note: the above examples introduced a new `meta` primitive: `meta:ref/1` (abbreviated to `?r/1` macro): this primitive exists because `meta` is in fact [hygienic macro system](http://en.wikipedia.org/wiki/Hygienic_macro) You can find the detailed description below but for now it is sufficient to say that `?r/1` also produces a quote (like `?q/1`) but it can only reference a variable already defined in scope and the name of this variable will be preserved when the quote from `?r/1` is spliced using `?s/1` (so `Arg` remains `Arg` inside `inc` call) while even though we could use `?q/1` instead of `?r/1` upon splicing `Arg` would be renamed to avoid name clashing with `Arg` in the function head so as a result we would get an error saing that variable `Arg1` is unbound and a warning that variable `Arg` is unused.
 
In fact we can go even further and inline `inc/1` function as well. If we turn it into meta-function then:

    q_inc(Arg) ->
        ?q(?s(Arg) + 1).
    
    triple_inc5(Arg) ->
        ?s(lists:foldl(
             fun(_, Q) -> q_inc(Q) end,
             ?r(Arg), lists:seq(1,3))).

Which produces the following code:

    triple_inc5(Arg) ->
        Arg + 1 + 1 + 1.

The resulting code can be further inspected and optimised if necessary.


Meta primitives
---------------
`TODO`
* meta:quote/1
* meta:splice/1
* meta:ref/1
* meta:verbatim/1
* meta:extract/1