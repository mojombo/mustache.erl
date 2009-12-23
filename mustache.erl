-module(mustache).  %% v0.1.0beta
-author("Tom Preston-Werner").
-export([render/2, run/1, val/3, start/1]).

-record(mstate, {mod = undefined,
                 section_re = undefined,
                 tag_re = undefined}).

render(Mod, File) ->
  code:load_file(Mod),
  {ok, TemplateBin} = file:read_file(File),
  State = #mstate{mod = Mod},
  CompiledTemplate = pre_compile(binary_to_list(TemplateBin), State),
  run(CompiledTemplate).

run(CompiledTemplate) ->
  io:format("~p~n", [CompiledTemplate]),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Out = lists:flatten(Fun()),
  io:format("~p~n", [Out]).

pre_compile(T, State) ->
  SectionRE = "\{\{\#([^\}]*)}}\s*(.+?){{\/\\1\}\}\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "\{\{(#|=|!|<|>|\{)?(.+?)\\1?\}\}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State2 = State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  "fun() -> Ctx = dict:new(), " ++ compile(T, State2) ++ " end.".

compile(T, State) ->
  Res = re:run(T, State#mstate.section_re),
  io:format("~p~n", [Res]),
  case Res of
    {match, [{M0, M1}, {N0, N1}, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Name = string:substr(T, N0 + 1, N1),
      Content = string:substr(T, C0 + 1, C1),
      "[" ++ compile_tags(Front, State) ++
        " | [" ++ compile_section(Name, Content, State) ++
        " | [" ++ compile(Back, State) ++ "]]]";
    nomatch ->
      "[" ++ compile_tags(T, State) ++ "]"
  end.

compile_section(Name, Content, State) ->
  Mod = State#mstate.mod,
  Result = compile(Content, State),
  "fun() -> " ++
    "Res = mustache:val(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ "), " ++
    "case Res of " ++
      "true -> " ++
        Result ++ "; " ++
      "false -> " ++
        "[]; " ++
      "List when is_list(List) -> " ++
        "[" ++ Result ++ " || X <- List] " ++
    "end " ++
  "end()".

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
  io:format("~p~n", [Res]),
  case Res of
    {match, [{M0, M1}, K, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Content = string:substr(T, C0 + 1, C1),
      Kind = tag_kind(T, K),
      Result = compile_tag(Kind, Content, State),
      "[\"" ++ Front ++ 
        "\" | [" ++ Result ++ 
        " | " ++ compile_tags(Back, State) ++ "]]";
    nomatch ->
      "[\"" ++ T ++ "\"]"
  end.

tag_kind(_T, {-1, 0}) ->
  none;
tag_kind(T, {K0, K1}) ->
  string:substr(T, K0 + 1, K1).

compile_tag(none, Content, State) ->
  Mod = State#mstate.mod,
  "mustache:val(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ ")";
compile_tag("!", _Content, _State) ->
  "[]".

val(Key, Ctx, Mod) when is_list(Key) ->
  val(list_to_atom(Key), Ctx, Mod);
val(Key, Ctx, Mod) ->
  case dict:find(Key, Ctx) of
    {ok, Val} -> Val;
    error ->
      case erlang:function_exported(Mod, Key, 0) of
        true ->
          apply(Mod, Key, []);
        false ->
          []
      end
  end.

%%---------------------------------------------------------------------------

start([T]) ->
  % T = "Hello {{name}}\nYou have just won ${{value}}!\n{{#in_ca}}\nWell, ${{ taxed_value }}, after taxes.\n{{/in_ca}}\n",
  % T = "abc {{#foo}} hi {{/foo}} def {{#bar}} bye {{/bar}} ghi\n",
  % T = "hello {{name}} you {{#in_ca}} DO {{/in_ca}} win {{value}}!",
  % D = compile(T),
  %io:format(D ++ "~n", []).
  render(list_to_atom(T), "examples/" ++ T ++ ".mustache").
      