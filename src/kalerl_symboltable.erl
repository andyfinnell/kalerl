%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_symboltable).

-export([new/0, add_scope/2, add_symbol/6, find_symbol/3, find_symbol_recursive/3]).
-export_type([symboltable/0]).

-record(symboltable, {next_scope = 0 :: integer(),
  scopes = #{} :: map()
}).

-record(scope, {parent :: kalerl_symbol:scopeid(),
  symbols = #{} :: map()
}).
-type symboltable() :: #symboltable{}.

%% Public API

-spec new() -> symboltable().
new() ->
  #symboltable{}.

-spec add_scope(kalerl_symbol:scopeid(), symboltable()) -> {kalerl_symbol:scopeid(), symboltable()}.
add_scope(ParentScopeID, Table = #symboltable{next_scope = NewScopeID, scopes = Scopes}) ->
  NewScope = #scope{parent = ParentScopeID},
  NewScopes = Scopes#{NewScopeID => NewScope},
  NewTable = Table#symboltable{next_scope = NewScopeID + 1, scopes = NewScopes},
  {NewScopeID, NewTable}.

-spec add_symbol(atom(), kalerl_symbol:symbol_type(), kalerl_symbol:scopeid(), kalerl_symbol:scopeid(), atom() | none, symboltable()) -> symboltable().
add_symbol(Id, Type, DefinedScope, Scope, Module, Table = #symboltable{scopes = Scopes}) ->
  Symbol = kalerl_symbol:new(Id, Type, Scope, DefinedScope, Module),
  OldScope = maps:get(Scope, Scopes),
  OldSymbols = OldScope#scope.symbols,
  NewSymbols = OldSymbols#{Id => Symbol},
  NewScope = OldScope#scope{symbols = NewSymbols},
  NewScopes = Scopes#{Scope => NewScope},
  Table#symboltable{scopes = NewScopes}.
  
-spec find_symbol(atom(), kalerl_symbol:scopeid(), symboltable()) -> {ok, kalerl_symbol:symbol()} | error.
find_symbol(Id, Scope, #symboltable{scopes = Scopes}) ->
  OldScope = maps:get(Scope, Scopes),
  maps:find(Id, OldScope#scope.symbols).
  
-spec find_symbol_recursive(atom(), kalerl_symbol:scopeid(), symboltable()) -> {ok, kalerl_symbol:symbol()} | error.
find_symbol_recursive(_Id, none, _Table) ->
  error;
find_symbol_recursive(Id, Scope, Table = #symboltable{scopes = Scopes}) ->
  OldScope = maps:get(Scope, Scopes),
  case maps:find(Id, OldScope#scope.symbols) of
    {ok, Symbol} -> {ok, Symbol};
    error -> find_symbol_recursive(Id, OldScope#scope.parent, Table)
  end.

%% Implementation
