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
  {ok, Contents} = file:read_file(filename:absname(Filename)),
  Tokens = process_lexing(kalerl_lexer:string(binary_to_list(Contents))),
  process_parse(kalerl_parser:parse(Tokens));
execute(_Args) ->
	usage().

process_lexing({ok, Tokens, _EndLine}) ->
  Tokens;
process_lexing({error, ErrorInfo, Line}) ->
	io:format("Error on line ~p: ~p ~n", [Line, ErrorInfo]),
  [].

process_parse({ok, Result}) ->
	io:format("AST:~n"),
  io:format("~p~n", [Result]);
process_parse({error, {Line, Module, Message}}) ->
  ErrorString = Module:format_error(Message),
	io:format("Error on line ~p: ~s ~n", [Line, ErrorString]).
  
-spec usage() -> ok.
usage() ->
	io:format("Usage: kalerl <path-to-compile>...~n"),
	ok.