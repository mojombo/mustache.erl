-module(mustache_ctx_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

new_ctx_empty_test() ->
    Ctx = mustache_ctx:new(),
    CtxList = mustache_ctx:to_list(Ctx),
    ?assertEqual([], CtxList).

new_ctx_from_proplist_test() ->
    List = [{k,v}],
    Ctx = mustache_ctx:new(List),
    CtxList = mustache_ctx:to_list(Ctx),
    ?assertEqual(List, CtxList).

new_ctx_from_improper_proplist_test() ->
    ?assertExit(_, mustache_ctx:new([{k,v}, other])).

module_not_set_test() ->
    Ctx = mustache_ctx:new(),
    ?assertEqual({error, module_not_set}, mustache_ctx:module(Ctx)).

module_set_and_get_test() ->
    Module = module_name,
    Ctx = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(Module, Ctx),
    ?assertEqual({ok, Module}, mustache_ctx:module(Ctx1)).

ctx_to_list_with_module_test() ->
    Module = module_name,
    Ctx = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(Module, Ctx),
    CtxList = mustache_ctx:to_list(Ctx1),
    ?assertEqual([], CtxList).

