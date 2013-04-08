-module(mustache_ctx).
-compile(export_all).

new() -> new([]).

new(Proplist) ->
    try dict:from_list(Proplist)
    catch
        _:_ -> exit({improper_ctx, Proplist})
    end.

to_list(Ctx) ->
    dict:to_list(Ctx).

