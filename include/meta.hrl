
-compile({parse_transform, meta}).

-define(q(Quote), (meta:quote(Quote))).
-define(s(Splice), (meta:splice(Splice))).
