-module(example).
-compile(export_all).

name() ->
  "Tom".

value() ->
  10000.

taxed_value() ->
  value() - (value() * 0.4).

in_ca() ->
  true.

%%---------------------------------------------------------------------------

start() ->
  CompiledTemplate = mustache:compile("simple.mustache"),
  Output = mustache:execute(CompiledTemplate),
  io:format(Output, []).