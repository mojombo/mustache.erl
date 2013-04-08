-module(mustache_ctx_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

new_ctx_empty_test() ->
    Ctx = mustache_ctx:new(),
    CtxList = mustache_ctx:to_list(Ctx),
    ?assertEqual([], CtxList).

new_ctx_proplist_test() ->
    List = [{k,v}],
    Ctx = mustache_ctx:new(List),
    CtxList = mustache_ctx:to_list(Ctx),
    ?assertEqual(List, CtxList).

new_ctx_improper_proplist_test() ->
    ?assertExit(_, mustache_ctx:new([{k,v}, other])).


