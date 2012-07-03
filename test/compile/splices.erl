-module(splices).

-compile(export_all).

-include("../../include/meta.hrl").

splice_base() ->
    meta:splice(meta:quote(42)).

integer() ->
    meta:quote(42).

    
