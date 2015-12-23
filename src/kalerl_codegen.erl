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
  convert_operator_result(Op, Line, {op, Line, operator(Op), expr(Expr1), expr(Expr2)});
expr({'if', Line, ConditionExpr, TrueExprs, FalseExprs}) ->
  FalseClause = {clause, Line, [{float, Line, 0.0}], [], body(FalseExprs)},
  TrueClause = {clause, Line, [{var, Line, '_'}], [], body(TrueExprs)},
  {'case', Line, expr(ConditionExpr), [FalseClause, TrueClause]};
expr({for, Line, IteratorName, InitExpr, EndExpr, StepExpr, BodyExprs}) ->
  LoopFunID = binary_to_atom(iolist_to_binary(io_lib:format("forloop__~s__~p__", [IteratorName, Line])), utf8),
  LoopVarExpr = {var, Line, LoopFunID},
  
  %% Essentially this is what we're generating for the for loop
  % Loop = fun Loop(IteratorName) ->
  %   case EndExpr of
  %     0.0 ->
  %       BodyExprs;
  %       Loop(IteratorName + StepExpr)
  %     _ -> 1.0;
  %   end,
  % Loop(InitExr)
  IteratorExpr = {var, Line, list_to_atom(IteratorName)},
  NextExpr = {op, Line, '+', IteratorExpr, expr(StepExpr)},
  RecurseExpr = {call, Line, LoopVarExpr, [NextExpr]},
  ContinueClause = {clause, Line, [{float, Line, 0.0}], [], body(BodyExprs) ++ [RecurseExpr]},
  StopClause = {clause, Line, [{var, Line, '_'}], [], [{float, Line, 1.0}]},
  CaseExpr = {'case', Line, expr(EndExpr), [ContinueClause, StopClause]},
  FunExpr = {named_fun, Line, LoopFunID, [{clause, Line, [IteratorExpr], [], [CaseExpr]}]},
  {call, Line, FunExpr, [expr(InitExpr)]};
expr({call, Line, Name, Args}) ->
  NameExpr = {atom, Line, list_to_atom(Name)},
  {call, Line, NameExpr, lists:map(fun expr/1, Args)}.

operator(Op) ->
  %% For now, there's a 1:1 correspondence
  Op.

convert_operator_result('<', Line, ExprForm) ->
  %% Our little language has to return float for all operators
  FalseClause = {clause, Line, [{atom, Line, false}], [], [{float, Line, 0.0}]},
  TrueClause = {clause, Line, [{atom, Line, true}], [], [{float, Line, 1.0}]},
  {'case', Line, ExprForm, [FalseClause, TrueClause]};  
convert_operator_result(_Op, _Line, Form) ->
  Form.

%% TODO: code generation
%%  - gen the -spec forms
