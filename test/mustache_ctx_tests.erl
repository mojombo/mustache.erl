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

new_ctx_from_dict_test() ->
    List = [{k,v}],
    Dict = dict:from_list(List),
    CtxFromList = mustache_ctx:new(List),
    CtxFromDict = mustache_ctx:new(Dict),
    ?assertEqual(CtxFromList, CtxFromDict).

new_ctx_from_improper_data_test() ->
    ?assertExit(_, mustache_ctx:new([{k,v}, other])),
    ?assertExit(_, mustache_ctx:new({other, tuple})),
    ?assertExit(_, mustache_ctx:new(other)).

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

get_from_empty_test() ->
    Ctx = mustache_ctx:new(),
    ?assertEqual({error, not_found}, mustache_ctx:get(key, Ctx)).

get_not_found_test() ->
    Ctx = mustache_ctx:new([{k,v}]),
    ?assertEqual({error, not_found}, mustache_ctx:get(key, Ctx)).

get_found_test() ->
    Ctx = mustache_ctx:new([{key,value}]),
    ?assertEqual({ok, value}, mustache_ctx:get(key, Ctx)).

get_from_module_not_found_test() ->
    Ctx0 = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(mock_module, Ctx0),
    ?assertEqual({error, not_found}, mustache_ctx:get(key, Ctx1)).

get_from_module_test_() ->
    {foreach,
        fun() -> ok = meck:new(mock_module) end,
        fun(_) -> ok = meck:unload(mock_module) end,
        [
            {"fun/1",               fun get_from_module_fun_1_/0},
            {"fun/0",               fun get_from_module_fun_0_/0},
            {"function call order", fun get_from_module_call_order_/0}
        ]}.

get_from_module_fun_1_() ->
    ok = meck:expect(mock_module, key, fun(_) -> value end),
    Ctx0 = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(mock_module, Ctx0),
    ?assertEqual({ok, value}, mustache_ctx:get(key, Ctx1)).

get_from_module_fun_0_() ->
    ok = meck:expect(mock_module, key, fun() -> value end),
    Ctx0 = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(mock_module, Ctx0),
    ?assertEqual({ok, value}, mustache_ctx:get(key, Ctx1)).

get_from_module_call_order_() ->
    ok = meck:expect(mock_module, key, fun(_) -> value_1 end),
    ok = meck:expect(mock_module, key, fun() -> value_0 end),
    Ctx0 = mustache_ctx:new(),
    Ctx1 = mustache_ctx:module(mock_module, Ctx0),
    ?assertEqual({ok, value_1}, mustache_ctx:get(key, Ctx1)).

merge_disjoin_test() ->
    Ctx1 = mustache_ctx:new([{k1,v1}]),
    Ctx2 = mustache_ctx:new([{k2,v2}]),
    Ctx = mustache_ctx:merge(Ctx1, Ctx2),
    ?assertEqual({ok, v1}, mustache_ctx:get(k1, Ctx)),
    ?assertEqual({ok, v2}, mustache_ctx:get(k2, Ctx)).

merge_intersecting_test() ->
    Ctx1 = mustache_ctx:new([{k0, v1}, {k1,v1}]),
    Ctx2 = mustache_ctx:new([{k0, v2}, {k2,v2}]),
    Ctx = mustache_ctx:merge(Ctx1, Ctx2),
    ?assertEqual({ok, v1}, mustache_ctx:get(k0, Ctx)),
    ?assertEqual({ok, v1}, mustache_ctx:get(k1, Ctx)),
    ?assertEqual({ok, v2}, mustache_ctx:get(k2, Ctx)).

