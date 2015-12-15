%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([eunit/1]).

all() ->
    [eunit].

groups() ->
	[].

eunit(_Config) ->
    ok = eunit:test({application, kalerl }).

%% Test Group callbacks
init_per_group(_, Config) ->
	Config.

end_per_group(_, _Config) ->
	ok.

%% Test Cases setup and teardown
init_per_testcase(_, Config) ->
	Config.

end_per_testcase(_, Config) ->
	Config.

%% Test Cases

