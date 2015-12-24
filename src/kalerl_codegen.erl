%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_codegen).

-export([module/3]).

-record(genstate, {scope :: kalerl_symbol:scopeid(),
  table :: kalerl_symboltable:symboltable()
}).

%% Public API

module({module, Line, ModuleName, Functions}, Scope, SymbolTable) ->
  State = #genstate{scope = Scope, table = SymbolTable},
  ModuleForm = {attribute, Line, module, list_to_atom(ModuleName)},
  {ok, [ModuleForm | functions(Functions, [], State)]}.
  
%% Implementation

functions([], Accumulator, _State) ->
  Accumulator;
functions([{function, _Line, _Prototype, _Exprs, Module} | Rest], Accumulator, State) when Module =/= none ->
  %% Skip over extern prototypes
  functions(Rest, Accumulator, State);
functions([Function | Rest], Accumulator, State) ->
  functions(Rest, [function(Function, State) | Accumulator], State).

function(F = {function, Line, Prototype, _Exprs, _Module}, State=#genstate{scope = Scope, table = Table}) ->
  Name = kalerl_ast:prototype_name(Prototype),
  Args = kalerl_ast:prototype_args(Prototype),
  {ok, Symbol} = kalerl_symboltable:find_symbol(list_to_atom(Name), Scope, Table),
  FunState = State#genstate{scope = kalerl_symbol:defined_scope(Symbol)},
  {function, Line, prototype_name_mangle(Prototype), length(Args), [function_clause(F, FunState)]}.

function_clause({function, Line, Prototype, Exprs, _Module}, State) ->
  Args = kalerl_ast:prototype_args(Prototype),
  {clause, Line, pattern_sequence(Args), [], body(Exprs, State)}.

pattern_sequence(Args) ->
  lists:map(fun pattern/1, Args).
    
pattern({variable, Line, Arg}) ->
  {var, Line, list_to_atom(Arg)}.

body(Exprs, State) ->
  Expr = fun (E) -> expr(E, State) end,
  lists:map(Expr, Exprs).
    
expr({number, Line, Number}, _State) ->
  {float, Line, Number};
expr({variable, Line, Name}, _State) ->
  {var, Line, list_to_atom(Name)};
expr(E = {binary, _Line, Op, _Expr1, _Expr2}, State) ->
  binary_op(kalerl_optable:is_builtin(Op), E, State);
expr({'if', Line, ConditionExpr, TrueExprs, FalseExprs}, State) ->
  FalseClause = {clause, Line, [{float, Line, 0.0}], [], body(FalseExprs, State)},
  TrueClause = {clause, Line, [{var, Line, '_'}], [], body(TrueExprs, State)},
  {'case', Line, expr(ConditionExpr, State), [FalseClause, TrueClause]};
expr({for, Line, IteratorName, InitExpr, EndExpr, StepExpr, BodyExprs}, State = #genstate{scope = Scope, table = Table}) ->
  {ok, Symbol} = kalerl_symboltable:find_symbol(list_to_atom(IteratorName), Scope, Table),
  LoopState = State#genstate{scope = kalerl_symbol:defined_scope(Symbol)},
  
  %% Essentially this is what we're generating for the for loop
  % Loop = fun Loop(IteratorName) ->
  %   case EndExpr of
  %     0.0 ->
  %       BodyExprs;
  %       Loop(IteratorName + StepExpr)
  %     _ -> 1.0;
  %   end,
  % Loop(InitExpr)
  LoopFunID = binary_to_atom(iolist_to_binary(io_lib:format("forloop__~s__~p__", [IteratorName, Line])), utf8),
  LoopVarExpr = {var, Line, LoopFunID},
  IteratorExpr = {var, Line, list_to_atom(IteratorName)},
  NextExpr = {op, Line, '+', IteratorExpr, expr(StepExpr, LoopState)},
  RecurseExpr = {call, Line, LoopVarExpr, [NextExpr]},
  ContinueClause = {clause, Line, [{var, Line, '_'}], [], body(BodyExprs, LoopState) ++ [RecurseExpr]},
  StopClause = {clause, Line, [{float, Line, 0.0}], [], [{float, Line, 1.0}]},
  CaseExpr = {'case', Line, expr(EndExpr, LoopState), [StopClause, ContinueClause]},
  FunExpr = {named_fun, Line, LoopFunID, [{clause, Line, [IteratorExpr], [], [CaseExpr]}]},
  {call, Line, FunExpr, [expr(InitExpr, State)]};
expr({call, Line, Name, Args}, State = #genstate{scope = Scope, table = Table}) ->
  FunID = list_to_atom(Name),
  {ok, Symbol} = kalerl_symboltable:find_symbol_recursive(FunID, Scope, Table),
  NameExpr = case kalerl_symbol:module(Symbol) of
    none -> {atom, Line, FunID};
    Module -> {remote, Line, {atom, Line, Module}, {atom, Line, FunID}}
  end,
  {call, Line, NameExpr, body(Args, State)}.

binary_op(true, {binary, Line, Op, Expr1, Expr2}, State) ->
  convert_operator_result(Op, Line, {op, Line, Op, expr(Expr1, State), expr(Expr2, State)});
binary_op(false, {binary, Line, Op, Expr1, Expr2}, State) ->
  OpID = binary_op_name_mangle(Op),
  {call, Line, {atom, Line, OpID}, body([Expr1, Expr2], State)}.

binary_op_name_mangle(Op) ->
  ToHex = fun (I) -> integer_to_list(I, 16) end,
  SafeOp = lists:flatten(lists:map(ToHex, atom_to_list(Op))),
  list_to_atom(string:concat("binary_", SafeOp)).
  
convert_operator_result('<', Line, ExprForm) ->
  %% Our little language has to return float for all operators
  FalseClause = {clause, Line, [{atom, Line, false}], [], [{float, Line, 0.0}]},
  TrueClause = {clause, Line, [{atom, Line, true}], [], [{float, Line, 1.0}]},
  {'case', Line, ExprForm, [FalseClause, TrueClause]};  
convert_operator_result(_Op, _Line, Form) ->
  Form.

prototype_name_mangle({prototype, _Line, Name, _FormalArgs}) ->
  list_to_atom(Name);
prototype_name_mangle({binop_prototype, _Line, Op, _Precedence, _Association, _FormalArgs}) ->
  binary_op_name_mangle(Op).


%% TODO: code generation
%%  - gen the -spec forms
