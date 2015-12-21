%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_codegen).

-define(KAL_LINE, 1).

-export([module/1]).

%% Public API

module({module, ModuleName, Functions}) ->
  ModuleForm = {attribute, ?KAL_LINE, module, list_to_atom(ModuleName)},
  {ok, [ModuleForm | functions(Functions, [])]}.
  
%% Implementation

functions([], Accumulator) ->
  Accumulator;
functions([{prototype, _Name, _Args} | Rest], Accumulator) ->
  functions(Rest, Accumulator);
functions([Function | Rest], Accumulator) ->
  functions(Rest, [function(Function) | Accumulator]).

function(F = {function, {prototype, Name, Args}, _Exprs}) ->
  {function, ?KAL_LINE, list_to_atom(Name), length(Args), [function_clause(F)]}.

function_clause({function, {prototype, _Name, Args}, Exprs}) ->
  {clause, ?KAL_LINE, pattern_sequence(Args), [], body(Exprs)}.

pattern_sequence(Args) ->
  lists:map(fun pattern/1, Args).
    
pattern(Arg) ->
  {var, ?KAL_LINE, list_to_atom(Arg)}.

body(Exprs) ->
  lists:map(fun expr/1, Exprs).
    
expr({number, Number}) ->
  {float, ?KAL_LINE, Number};
expr({variable, Name}) ->
  {var, ?KAL_LINE, list_to_atom(Name)};
expr({binary, Op, Expr1, Expr2}) ->
  {op, ?KAL_LINE, operator(Op), expr(Expr1), expr(Expr2)};
expr({call, Name, Args}) ->
  NameExpr = {atom, ?KAL_LINE, list_to_atom(Name)},
  {call,?KAL_LINE, NameExpr, lists:map(fun expr/1, Args)}.

operator(Op) ->
  %% For now, there's a 1:1 correspondence
  Op.
  
%% TODO: symbol checking:
%%  - duplicate parameter names
%%  - undefined symbols used in exprs
%% TODO: gen the -spec forms
