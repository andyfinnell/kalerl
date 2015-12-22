%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_symbol).

%% Public API

-type symbol_type() :: float | {function, integer()}.
-type scopeid() :: integer() | none.
-record(symbol, {name :: atom(),
  type :: symbol_type(),
  parent_scope :: scopeid(),
  defined_scope :: scopeid()
}).
-type symbol() :: #symbol{}.

-export_type([symbol_type/0, scopeid/0, symbol/0]).
-export([new/4, type/1, parent_scope/1, defined_scope/1]).

-spec new(atom(), symbol_type(), scopeid(), scopeid()) -> symbol().
new(Name, Type, ParentScope, DefinedScope) ->
  #symbol{name = Name, type = Type, parent_scope = ParentScope, defined_scope = DefinedScope}.

-spec type(symbol()) -> symbol_type().
type(#symbol{type = Type}) -> Type.

-spec parent_scope(symbol()) -> scopeid().
parent_scope(#symbol{parent_scope = Scope}) -> Scope.

-spec defined_scope(symbol()) -> scopeid().
defined_scope(#symbol{defined_scope = Scope}) -> Scope.

%% Implementation

