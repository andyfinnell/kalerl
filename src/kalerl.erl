%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl).

-export([main/1]).

%% Public API

-spec main(list()) -> ok.
main(Args) ->			
	execute(Args).

%% Implementation

-spec execute(list()) -> ok.
execute(_Args) ->
	usage().


-spec usage() -> ok.
usage() ->
	io:format("Usage: kalerl <path-to-compile>...~n"),
	ok.