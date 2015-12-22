%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_codegen).

-export([module/1]).

%% Public API

module({module, Line, ModuleName, Functions}) ->
  ModuleForm = {attribute, Line, module, list_to_atom(ModuleName)},
  {ok, [ModuleForm | functions(Functions, [])]}.
  
%% Implementation

functions([], Accumulator) ->
  Accumulator;
functions([{prototype, _Line, _Name, _Args} | Rest], Accumulator) ->
  functions(Rest, Accumulator);
functions([Function | Rest], Accumulator) ->
  functions(Rest, [function(Function) | Accumulator]).

function(F = {function, Line, {prototype, _Line, Name, Args}, _Exprs}) ->
  {function, Line, list_to_atom(Name), length(Args), [function_clause(F)]}.

function_clause({function, Line, {prototype, _Line, _Name, Args}, Exprs}) ->
  {clause, Line, pattern_sequence(Args), [], body(Exprs)}.

pattern_sequence(Args) ->
  lists:map(fun pattern/1, Args).
    
pattern({variable, Line, Arg}) ->
  {var, Line, list_to_atom(Arg)}.

body(Exprs) ->
  lists:map(fun expr/1, Exprs).
    
expr({number, Line, Number}) ->
  {float, Line, Number};
expr({variable, Line, Name}) ->
  {var, Line, list_to_atom(Name)};
expr({binary, Line, Op, Expr1, Expr2}) ->
  {op, Line, operator(Op), expr(Expr1), expr(Expr2)};
expr({call, Line, Name, Args}) ->
  NameExpr = {atom, Line, list_to_atom(Name)},
  {call, Line, NameExpr, lists:map(fun expr/1, Args)}.

operator(Op) ->
  %% For now, there's a 1:1 correspondence
  Op.
  
%% TODO: symbol checking:
%%  - duplicate parameter names
%%  - undefined symbols used in exprs
%%  - duplicate function names (with same arity)
%% TODO: code generation
%%  - gen the -spec forms
%%  - gen correct line numbers
