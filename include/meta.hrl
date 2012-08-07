
-compile({parse_transform, meta}).

-define(q(Quote), (meta:quote(Quote))).
-define(qv(Quote), (meta:quote_verbatim(Quote))).
-define(s(Splice), (meta:splice(Splice))).
-define(sv(Splice), (meta:splice_verbatim(Splice))).
