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

-module(mustache_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Ctx = dict:from_list([{name, "world"}]),
    Result = mustache:render("Hello {{name}}!", Ctx),
    ?assertEqual("Hello world!", Result).

integer_values_too_test() ->
    Ctx = dict:from_list([{name, "Chris"}, {value, 10000}]),
    Result = mustache:render("Hello {{name}}~nYou have just won ${{value}}!", Ctx),
    ?assertEqual("Hello Chris~nYou have just won $10000!", Result).

specials_test() ->
    Ctx = dict:from_list([{name, "Chris"}, {value, 10000}]),
    Result = mustache:render("\'Hello\n\"{{name}}\"~nYou \"have\" ju\0st\\ won\b\r\"${{value}}!\"\t", Ctx),
    ?assertEqual("\'Hello\n\"Chris\"~nYou \"have\" ju\0st\\ won\b\r\"$10000!\"\t", Result).
