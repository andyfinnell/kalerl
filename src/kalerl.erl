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
execute([Filename]) ->  
  process_parse(kalerl_compiler:file(Filename));
execute(_Args) ->
	usage().

process_parse({ok, Result}) ->
	io:format("AST:~n"),
  io:format("~p~n", [Result]);
process_parse({error, Line, Message}) ->
	io:format("Error on line ~p: ~s ~n", [Line, Message]).
  
-spec usage() -> ok.
usage() ->
	io:format("Usage: kalerl <path-to-compile>...~n"),
	ok.