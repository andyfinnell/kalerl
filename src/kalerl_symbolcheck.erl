%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_symbolcheck).

-export([module/1, format_error/1]).

-record(checkstate, {table :: kalerl_symboltable:symboltable(),
  scope :: kalerl_symboltable:scopeid(),
  errors :: list(),
  warnings :: list()
}).

%% Public API

module(Module) ->  
  State1 = pass1_module(Module),
  State2 = pass2_module(Module, State1),
  process_state(Module, State2).

format_error({undefined, Name}) ->
  io_lib:format("undefined symbol '~s'", [Name]);
format_error({type_mismatch, ActualType, ExpectedType}) ->
  io_lib:format("type mismatch; was expecting ~p, got ~p", [ExpectedType, ActualType]);
format_error({redefine, Name}) ->
  io_lib:format("symbol '~s' redefined", [Name]).

%% Implementation

process_state(Module, #checkstate{errors = Errors}) when length(Errors) =:= 0 ->
  {ok, Module};
process_state(_Module, #checkstate{errors = Errors, warnings = Warnings}) ->
  {error, Errors, Warnings}.

%% Pass 1 where we define symbols

pass1_module({module, _Line, _ModuleName, Functions}) ->
  {ScopeID, Table} = kalerl_symboltable:add_scope(none, kalerl_symboltable:new()),
  State = #checkstate{table = Table, scope = ScopeID, errors = [], warnings = []},
  lists:foldl(fun pass1_function/2, State, Functions).

pass1_function({prototype, _Line, _Name, _Args}, State) ->
  %% We don't check extern prototypes
  State; 
pass1_function({function, Line, {prototype, _Line, Name, Args}, Exprs}, State = #checkstate{scope = OldScopeID}) ->
  State1 = define_scope_symbol(Name, Line, {function, length(Args)}, State),  
  State2 = lists:foldl(fun pass1_parameter/2, State1, Args),
  State3 = lists:foldl(fun pass1_expr/2, State2, Exprs),
  State3#checkstate{scope = OldScopeID}.
  
pass1_parameter({variable, Line, Name}, State) ->
  define_symbol(Name, Line, float, none, State).  
    
pass1_expr({number, _Line, _Number}, State) ->
  State;
pass1_expr({variable, Line, Name}, State = #checkstate{scope = ScopeID, table = Table, errors = Errors}) ->
  Id = list_to_atom(Name),
  case kalerl_symboltable:find_symbol(Id, ScopeID, Table) of
    {ok, _Symbol} -> State;
    error ->
      Error = {Line, ?MODULE, {undefined, Name}},
      State#checkstate{errors = [Error | Errors]}
  end;
pass1_expr({binary, _Line, _Op, Expr1, Expr2}, State) ->
  lists:foldl(fun pass1_expr/2, State, [Expr1, Expr2]);
pass1_expr({call, _Line, _Name, Args}, State) ->
  lists:foldl(fun pass1_expr/2, State, Args).

check_type(ActualType, ExpectedType, _Line, State) when ActualType =:= ExpectedType ->
  State;
check_type(ActualType, ExpectedType, Line, State = #checkstate{errors = Errors}) ->
  Error = {Line, ?MODULE, {type_mismatch, ActualType, ExpectedType}},
  State#checkstate{errors = [Error | Errors]}.
  
define_scope_symbol(Name, Line, Type, State = #checkstate{scope = ParentScope, table = Table}) ->
  {NewScopeID, NewTable} = kalerl_symboltable:add_scope(ParentScope, Table),
  State1 = State#checkstate{table = NewTable},
  State2 = define_symbol(Name, Line, Type, NewScopeID, State1),
  State2#checkstate{scope = NewScopeID}.
  
define_symbol(Name, Line, Type, DefinedScope, State = #checkstate{scope = ParentScope, table = Table, errors = Errors}) ->
  SymbolID = list_to_atom(Name),
  case kalerl_symboltable:find_symbol(SymbolID, ParentScope, Table) of
    {ok, _Symbol} -> 
      Error = {Line, ?MODULE, {redefine, Name}},
      State#checkstate{errors = [Error | Errors]};
    error -> 
      NewTable = kalerl_symboltable:add_symbol(SymbolID, Type, DefinedScope, ParentScope, Table),
      State#checkstate{table = NewTable}
  end.

%% Pass 2 where we verify function calls

pass2_module({module, _Line, _ModuleName, Functions}, State) ->
  lists:foldl(fun pass2_function/2, State, Functions).

pass2_function({prototype, _Line, _Name, _Args}, State) ->
  %% We don't check extern prototypes
  State; 
pass2_function({function, _Line, {prototype, _Line, Name, _Args}, Exprs}, State = #checkstate{scope = OldScopeID, table = Table}) ->
  FunctionID = list_to_atom(Name),
  {ok, Symbol} = kalerl_symboltable:find_symbol(FunctionID, OldScopeID, Table),
  State1 = State#checkstate{scope = kalerl_symbol:defined_scope(Symbol)},
  State2 = lists:foldl(fun pass2_expr/2, State1, Exprs),
  State2#checkstate{scope = OldScopeID}.
      
pass2_expr({number, _Line, _Number}, State) ->
  State;
pass2_expr({variable, _Line, _Name}, State) ->
  State;
pass2_expr({binary, _Line, _Op, Expr1, Expr2}, State) ->
  lists:foldl(fun pass2_expr/2, State, [Expr1, Expr2]);
pass2_expr({call, Line, Name, Args}, State = #checkstate{scope = ScopeID, table = Table, errors = Errors}) ->
  FunctionID = list_to_atom(Name),
  case kalerl_symboltable:find_symbol_recursive(FunctionID, ScopeID, Table) of
    {ok, Symbol} -> 
      ExpectedType = {function, length(Args)},
      check_type(kalerl_symbol:type(Symbol), ExpectedType, Line, State);
    error ->
      Error = {Line, ?MODULE, {undefined, Name}},
      State#checkstate{errors = [Error | Errors]}
  end.