%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
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

-module(mustache_ctx).

-define(MODULE_KEY, '__mod__').
-define(THIS_KEY, '__this__').
-define(NEW_EXIT(Data), exit({improper_ctx, Data})).

-export([ new/0, new/1, to_list/1 ]).
-export([ merge/2 ]).
-export([ module/1, module/2 ]).
-export([ get/2 ]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

%% ===================================================================
%% Create new context
%% ===================================================================

new() -> #{}.

new(List) when is_list(List) ->
    try maps:from_list(List)
    catch
        _:_ -> ?NEW_EXIT(List)
    end;
new(Data) when is_tuple(Data) ->
    case erlang:element(1, Data) of
        dict ->
            try maps:from_list(dict:to_list(Data))
            catch
                _:_ -> ?NEW_EXIT(Data)
            end;
        _ -> ?NEW_EXIT(Data)
    end;
new(Map) when is_map(Map) ->
    Map;
new(Data) ->
    ?NEW_EXIT(Data).

to_list(Ctx) ->
    List = maps:to_list(Ctx),
    lists:keydelete(?MODULE_KEY, 1, List).

%% ===================================================================
%% Merge
%% ===================================================================

merge(Ctx1, Ctx2) when is_map(Ctx1), is_map(Ctx2) ->
    maps:merge(Ctx2, Ctx1);
merge(This, Ctx) when is_map(Ctx) ->
    maps:put(?THIS_KEY, This, Ctx).

%% ===================================================================
%% Dynamic data module
%% ===================================================================

module(Ctx) ->
    case maps:find(?MODULE_KEY, Ctx) of
        {ok, Module} -> {ok, Module};
        error -> {error, module_not_set}
    end.

module(Module, Ctx) ->
    maps:put(?MODULE_KEY, Module, Ctx).

%% ===================================================================
%% Module
%% ===================================================================

get([], Ctx) ->
    {ok, maps:get(?THIS_KEY, Ctx)};
get(Path, Ctx) when is_list(Path) ->
    case get_path(Path, Ctx) of
        {ok, Value} -> {ok, Value};
        error       -> get_from_module(Path, Ctx)
    end.

get_path([], Value) ->
    {ok, Value};
get_path([Key | Keys], Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> get_path(Keys, Value);
        error       -> error
    end.

get_from_module([Key | Rest], Ctx) ->
    FunList = case module(Ctx) of
        {error, _} -> [];
        {ok, Module} -> [
                fun() -> get_path(Rest, Module:Key(Ctx)) end,
                fun() -> get_path(Rest, Module:Key()) end
            ]
    end,
    get_from_module(FunList).

get_from_module([]) -> {error, not_found};
get_from_module([ Fun | Rest ]) ->
    try Fun()
    catch
        _:_ ->
        get_from_module(Rest)
    end.

