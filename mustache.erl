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
  io:format("~p~n~n", [CompiledTemplate]),
  io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  lists:flatten(Fun()).

pre_compile(T, State) ->
  SectionRE = "\{\{\#([^\}]*)}}\s*(.+?){{\/\\1\}\}\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "\{\{(#|=|!|<|>|\{)?(.+?)\\1?\}\}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State2 = State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  "fun() -> " ++
    "Ctx = dict:new(), " ++ 
    "CFun = fun(A, B) -> A end, " ++
    compile(T, State2) ++ " end.".

compile(T, State) ->
  Res = re:run(T, State#mstate.section_re),
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
      compile_tags(T, State)
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
        "LFun = fun(Ctx) -> " ++ Result ++ " end, " ++
        "[LFun(dict:merge(CFun, SubCtx, Ctx)) || SubCtx <- List] " ++
    "end " ++
  "end()".

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
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
    {ok, Val} -> to_s(Val);
    error ->
      case erlang:function_exported(Mod, Key, 0) of
        true ->
          to_s(apply(Mod, Key, []));
        false ->
          []
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

%%---------------------------------------------------------------------------

start([T]) ->
  Out = render(list_to_atom(T), "examples/" ++ T ++ ".mustache"),
  io:format("~p~n", [Out]).
      