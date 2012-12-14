
-compile({parse_transform, meta}).

-define(q(Quote), (meta:quote(Quote))).
-define(r(Ref), (meta:ref(Ref))).
-define(s(Splice), (meta:splice(Splice))).
-define(v(Verbatim), (meta:verbatim(Verbatim))).

-define(e(Quote), element(1, (Quote)(gb_sets:new()))).
-define(i(Splice), (meta:extract(Splice))).
