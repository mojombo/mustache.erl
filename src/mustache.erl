%% The MIT License
%% 
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%%               & 2010 Steven Gravell <steve@mokele.co.uk>
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache).
-author("Tom Preston-Werner").
-author("Steven Gravell").
-vsn("0.2.0").
-export([compile/1, compile/2, render/1, render/2, render/3, get/2, get/3, escape/1, start/1]).

-record(mstate, {mod = undefined, binmod = <<"undefined">>}).

compile(Body) when is_list(Body) ->
  State = #mstate{},
  CompiledTemplate = pre_compile(Body, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun;
compile(Mod) ->
  TemplatePath = template_path(Mod),
  compile(Mod, TemplatePath).

compile(Mod, File) ->
  code:purge(Mod),
  code:load_file(Mod),
  {ok, TemplateBin} = file:read_file(File),
  State = #mstate{mod = Mod, binmod = atom_to_binary(Mod,utf8)},
  CompiledTemplate = pre_compile(TemplateBin, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format("File: ~p" ++ CompiledTemplate ++ "~n", [File]),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

render(Mod) ->
  TemplatePath = template_path(Mod),
  render(Mod, TemplatePath).

render(Body, Ctx) when is_list(Body) ->
  TFun = compile(Body),
  render(undefined, TFun, Ctx);
render(Mod, File) when is_list(File) ->
  render(Mod, File, dict:new());
render(Mod, CompiledTemplate) ->
  render(Mod, CompiledTemplate, dict:new()).

render(Mod, File, Ctx) when is_list(File) ->
  CompiledTemplate = compile(Mod, File),
  render(Mod, CompiledTemplate, Ctx);
render(Mod, CompiledTemplate, Ctx) ->
  Ctx2 = dict:store('__mod__', Mod, Ctx),
  lists:flatten(CompiledTemplate(Ctx2)).

pre_compile(T, State) ->
  Compiled = compiler(T, State),
  binary_to_list(
  <<"fun(Ctx) -> ",
      "CFun = fun(_K, A, B) -> A end, ",
      Compiled/binary,
    " end.">>).

unreel_stack(Stack) ->
  unreel_stack(lists:reverse(Stack), <<>>).
unreel_stack([], Bin) ->
  Bin;
unreel_stack([H|T], Bin) ->
  unreel_stack(T, <<Bin/binary,H/binary>>).

compiler(T, State) ->
  compiler(T, <<>>, [], State).

compiler(<<>>, Buf, Stack, _State) ->
  Unreeled = unreel_stack(Stack),
  <<$",Buf/binary,Unreeled/binary,$">>;

compiler(<<$"/utf8,R/binary>>, Buf, Stack, State) ->
  compiler(R, <<Buf/binary,$\\/utf8,$"/utf8>>, Stack, State);

compiler(<<${/utf8,${/utf8,${/utf8,R/binary>>, Buf, Stack, State) ->
  maybe_tag(R, <<>>, [Buf|Stack], State);
compiler(<<${/utf8,${/utf8,R/binary>>, Buf, Stack, State) ->
  maybe_tag(R, <<>>, [Buf|Stack], State);

compiler(<<C/utf8,R/binary>>, Buf, Stack, State) ->
  compiler(R, <<Buf/binary,C/utf8>>, Stack, State).

maybe_tag(<<$}/utf8,$}/utf8,$}/utf8,R/binary>>, Buf, [SoFar|Stack], State) ->
  BinMod = State#mstate.binmod,
  Compiled = <<"\"++mustache:get('",Buf/binary,"', Ctx, ",BinMod/binary,")++\"">>,
  compiler(R, <<SoFar/binary,Compiled/binary>>, Stack, State);
maybe_tag(<<$}/utf8,$}/utf8,R/binary>>, Buf, [SoFar|Stack], State) ->
  BinMod = State#mstate.binmod,
  Compiled = <<"\"++mustache:escape(mustache:get('",Buf/binary,"', Ctx, ",BinMod/binary,"))++\"">>,
  compiler(R, <<SoFar/binary,Compiled/binary>>, Stack, State);

maybe_tag(<<$^/utf8,R/binary>>, <<>>, Stack, State) ->
  {R2,Tag} = tag_name(R),
  compiler(R2, <<>>, [{negative,Tag}|Stack], State);
maybe_tag(<<$#/utf8,R/binary>>, <<>>, Stack, State) ->
  {R2,Tag} = tag_name(R),
  compiler(R2, <<>>, [Tag|Stack], State);

maybe_tag(<<$//utf8,R/binary>>, <<>>, [Content|[{negative,_StartTag}|[SoFar|Stack]]], State) ->
  {R2,Tag} = tag_name(R),
  %% TODO: compare Tag and StartTag to provide a nice debug msg
  BinMod = State#mstate.binmod,
  Compiled = <<"\" ++ fun() -> ",
    "case mustache:get('",Tag/binary,"', Ctx, ",BinMod/binary,") of ",
      "[] -> ",
        "\"",Content/binary,"\"; ",
      "false -> ",
        "\"",Content/binary,"\"; ",
      "_ -> ",
        "[]",
    "end ",
  "end() ++ \"">>,
  compiler(R2, <<SoFar/binary,Compiled/binary>>, Stack, State);

maybe_tag(<<$//utf8,R/binary>>, <<>>, [Content|[_StartTag|[SoFar|Stack]]], State) ->
  {R2,Tag} = tag_name(R),
  %% TODO: compare Tag and StartTag to provide a nice debug msg
  BinMod = State#mstate.binmod,
  Compiled = <<"\" ++ fun() -> ",
    "case mustache:get('",Tag/binary,"', Ctx, ",BinMod/binary,") of ",
      "true -> ",
        "\"",Content/binary,"\"; ",
      "[] -> ",
        "[];",
      "false -> ",
        "[];",
      "List when is_list(List) -> ",
        "[fun(Ctx) -> \"",Content/binary,"\" end(dict:merge(CFun, SubCtx, Ctx)) || SubCtx <- List]; ",
      "_ -> ",
        "throw({template, io_lib:format(\"Bad context for ~p\", ['",Tag/binary,"'])}) ",
    "end ",
  "end() ++ \"">>,
  compiler(R2, <<SoFar/binary,Compiled/binary>>, Stack, State);
maybe_tag(<<$!/utf8,R/binary>>, <<>>, [SoFar|Stack], State) ->
  comment(R, SoFar, Stack, State);
maybe_tag(<<${/utf8,R/binary>>, <<>>, [SoFar|Stack], State) ->
  compiler(R, <<SoFar/binary,${/utf8,${/utf8,${/utf8>>, Stack, State);
maybe_tag(<<" "/utf8,R/binary>>, Buf, Stack, State) ->
  maybe_tag(R, Buf, Stack, State);
maybe_tag(<<C/utf8,R/binary>>, Buf, Stack, State) ->
  maybe_tag(R, <<Buf/binary,C/utf8>>, Stack, State).

comment(<<$}/utf8,$}/utf8,R/binary>>, Buf, Stack, State) ->
  compiler(R, Buf, Stack, State);
comment(<<_/utf8,R/binary>>, Buf, Stack, State) ->
  comment(R, Buf, Stack, State).

tag_name(R) ->
  tag_name(R, <<>>).
tag_name(<<$}/utf8,$}/utf8,R/binary>>, Name) ->
  {R,Name};
tag_name(<<" "/utf8,R/binary>>, Name) ->
  tag_name(R, Name);
tag_name(<<C/utf8,R/binary>>, Name) ->
  tag_name(R, <<Name/binary,C/utf8>>).

template_path(Mod) ->
  ModPath = code:which(Mod),
  re:replace(ModPath, "\.beam$", ".mustache", [{return, list}]).

get(Key, Ctx) when is_list(Key) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(Key, Ctx, Mod).

get(Key, Ctx, Mod) when is_list(Key) ->
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx, Mod) ->
  case dict:find(Key, Ctx) of
    {ok, Val} ->
      % io:format("From Ctx {~p, ~p}~n", [Key, Val]),
      to_s(Val);
    error ->
      case erlang:function_exported(Mod, Key, 1) of
        true ->
          Val = to_s(apply(Mod, Key, [Ctx])),
          % io:format("From Mod/1 {~p, ~p}~n", [Key, Val]),
          Val;
        false ->
          case erlang:function_exported(Mod, Key, 0) of
            true ->
              Val = to_s(apply(Mod, Key, [])),
              % io:format("From Mod/0 {~p, ~p}~n", [Key, Val]),
              Val;
            false ->
              []
          end
      end
  end.

to_s(Val) when is_integer(Val) ->
  integer_to_list(Val);
to_s(Val) when is_float(Val) ->
  io_lib:format("~.2f", [Val]);
to_s(Val) when is_boolean(Val) ->
  Val;
to_s(Val) when is_atom(Val) ->
  atom_to_list(Val);
to_s(Val) ->
  Val.

escape(HTML) ->
  escape(HTML, []).

escape([], Acc) ->
  lists:reverse(Acc);
escape([$< | Rest], Acc) ->
  escape(Rest, lists:reverse("&lt;", Acc));
escape([$> | Rest], Acc) ->
  escape(Rest, lists:reverse("&gt;", Acc));
escape([$& | Rest], Acc) ->
  escape(Rest, lists:reverse("&amp;", Acc));
escape([X | Rest], Acc) ->
  escape(Rest, [X | Acc]).

%%---------------------------------------------------------------------------

start([T]) ->
  Out = render(list_to_atom(T)),
  io:format(Out ++ "~n", []).
      
