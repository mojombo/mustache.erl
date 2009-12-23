-module(mustache).  %% v0.1.0beta
-author("Tom Preston-Werner").
-export([render/2, compile/1, start/0]).

-record(mstate, {section_re = undefined,
                 tag_re = undefined}).

render(Mod, F) -> ok.
%   {ok, TemplateBin} = file:read_file(F),
%   CompiledTemplate = compile(binary_to_list(TemplateBin)),
  

compile(T) ->
  SectionRE = "\{\{\#([^\}]*)}}\s*(.+?){{\/\\1\}\}\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "\{\{(#|=|!|<|>|\{)?(.+?)\\1?\}\}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State = #mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  compile(T, State).

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
  Result = compile(Content, State),
  % "<" ++ Name ++ ">" ++ Result ++ "</" ++ Name ++ ">".
  "fun() -> " ++ Result ++ " end()".

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
  io:format("~p~n", [Res]),
  case Res of
    {match, [{M0, M1}, K, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Content = string:substr(T, C0 + 1, C1),
      Kind = tag_kind(T, K),
      Result = compile_tag(Kind, Content),
      "[\"" ++ Front ++ 
        "\" | [" ++ Result ++ 
        " | " ++ compile_tags(Back, State) ++ "]]";
    nomatch ->
      "[\"" ++ T ++ "\"]"
  end.

tag_kind(T, {-1, 0}) ->
  none;
tag_kind(T, {K0, K1}) ->
  string:substr(T, K0 + 1, K1).

compile_tag(none, Content) ->
  "apply(simple, " ++ Content ++ ", [])";
compile_tag("!", _Content) ->
  "[]".

%%---------------------------------------------------------------------------

start() ->
  % T = "Hello {{name}}\nYou have just won ${{value}}!\n{{#in_ca}}\nWell, ${{ taxed_value }}, after taxes.\n{{/in_ca}}\n",
  % T = "abc {{#foo}} hi {{/foo}} def {{#bar}} bye {{/bar}} ghi\n",
  T = "hello {{!name}} you {{#in_ca}} DO {{/in_ca}} win {{value}}!",
  D = compile(T),
  io:format(D, []).
      