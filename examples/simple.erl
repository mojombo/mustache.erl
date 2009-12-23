-module(simple).
-compile(export_all).

name() ->
  "Tom".

value() ->
  "10000".

taxed_value() ->
  integer_to_list(value() - (value() * 0.4)).

in_ca() ->
  true.

%%---------------------------------------------------------------------------

% start() ->
%   code:add_patha(".."),
%   Output = mustache:render(simple, "simple.mustache"),
%   io:format(Output, []).