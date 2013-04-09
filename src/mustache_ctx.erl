-module(mustache_ctx).
-compile(export_all).

-define(MODULE_KEY, '__mod__').

new() -> new([]).

new(Proplist) ->
    try dict:from_list(Proplist)
    catch
        _:_ -> exit({improper_ctx, Proplist})
    end.

to_list(Ctx) ->
    List = dict:to_list(Ctx),
    lists:keydelete(?MODULE_KEY, 1, List).

module(Ctx) ->
    case dict:find(?MODULE_KEY, Ctx) of
        {ok, Module} -> {ok, Module};
        error -> {error, module_not_set}
    end.

module(Module, Ctx) ->
    dict:store(?MODULE_KEY, Module, Ctx).

get(Key, Ctx) ->
    case dict:find(Key, Ctx) of
        {ok, Value} -> {ok, Value};
        error ->
            get_from_module(Key, Ctx)
    end.

get_from_module(Key, Ctx) ->
    FunList = case module(Ctx) of
        {error, _} -> [];
        {ok, Module} -> [
                fun() -> Module:Key(Ctx) end,
                fun() -> Module:Key() end
            ]
    end,
    get_from_module(FunList).

get_from_module([]) -> {error, not_found};
get_from_module([ Fun | Rest ]) ->
    try Value = Fun(),
        {ok, Value}
    catch
        _:_ ->
        get_from_module(Rest)
    end.

