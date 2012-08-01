
-record(attribute, {line, name, arg}).
-record(function, {line, name, arity, clauses}).
-record(call, {line, function, args}).
-record(remote, {line, module, name}).
-record(op, {line, name, arg1, arg2}).
-record(var, {line, name}).
-record(atom, {line, name}).
