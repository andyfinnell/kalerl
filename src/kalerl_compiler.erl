%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_compiler).

-compile({parse_transform, do}).

%% Public API

-export([file/1]).

-spec file(file:filename_all()) -> 
  {ok, kalerl_ast:kalerl_module()} | {error, integer(), string()}.
file(Filename) ->
  do([error_m ||
    Contents <- read_error(file:read_file(filename:absname(Filename))),
    Tokens <- lexer_error(kalerl_lexer:string(binary_to_list(Contents))),
    Toplevel <- parser_error(kalerl_parser:parse(Tokens)),
    IRModule <- kalerl_parser:toplevel_to_module(Toplevel, filename:rootname(filename:basename(Filename))),
    kalerl_codegen:module(IRModule)
  ]).
    
%% Implementation

read_error({error, _Reason}) ->
  {error, 0, "Could not read file"};
read_error(Passthrough) ->
  Passthrough.

lexer_error({ok, Tokens, _EndLine}) ->
  {ok, Tokens};
lexer_error({error, ErrorInfo, Line}) ->
  {error, Line, ErrorInfo}.
  
parser_error({error, {Line, Module, Message}}) ->
  ErrorString = Module:format_error(Message),
  {error, Line, ErrorString};
parser_error(Passthrough) ->
  Passthrough.
