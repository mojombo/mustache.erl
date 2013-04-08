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

