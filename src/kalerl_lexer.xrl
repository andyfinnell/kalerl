%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

Definitions.

DIGIT = [0-9]
LETTER = [A-Za-z]
WHITESPACE = [\r\n\t\s]

Rules.

{LETTER}({LETTER}|{DIGIT})*   : {token, validate_ident(TokenLine, TokenChars)}.
{DIGIT}+(\.{DIGIT}+)?         : {token, validate_number(TokenLine, TokenChars)}.
{WHITESPACE}+                 : skip_token.
#.*                           : skip_token. %% comments
[\(\),]                       : {token, validate_char(TokenLine, TokenChars)}.
.                             : {token, validate_operator(TokenLine, TokenChars)}.

Erlang code.

-type lineno() :: integer().
-type kalerl_token() :: {def, lineno()} | {extern, lineno()} %% Commands
    %% Primary
    | {ident, lineno(), string()} | {number, lineno(), float()} 
    %% Special literal character
    | {special, lineno(), char()}.
    

-spec validate_ident(lineno(), string()) -> kalerl_token().
validate_ident(Line, "def") -> {def, Line};
validate_ident(Line, "extern") -> {extern, Line};
validate_ident(Line, Characters) -> {ident, Line, Characters}.

-spec validate_number(lineno(), string()) -> kalerl_token().
validate_number(Line, Characters) ->
  try list_to_float(Characters) of
    Float -> {number, Line, Float}
  catch
    error:badarg ->
      {number, Line, float(list_to_integer(Characters))}
  end.

-spec validate_operator(lineno(), string()) -> kalerl_token().
validate_operator(Line, Chars) -> {operator, Line, list_to_atom(Chars)}.

-spec validate_char(lineno(), string()) -> kalerl_token().
validate_char(Line, Chars) -> {list_to_atom(Chars), Line}.
