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
  process_lexing(kalerl_lexer:string(binary_to_list(Contents)));
execute(_Args) ->
	usage().

process_lexing({ok, Tokens, _EndLine}) ->
	io:format("Tokens:~n"),
  PrintToken = fun (Token) -> io:format("~p~n", [Token]) end,
  lists:foreach(PrintToken, Tokens);
process_lexing({error, ErrorInfo, Line}) ->
	io:format("Error on line ~p: ~p ~n", [Line, ErrorInfo]).

-spec usage() -> ok.
usage() ->
	io:format("Usage: kalerl <path-to-compile>...~n"),
	ok.