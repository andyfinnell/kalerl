%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_ast).

-export([prototype_name/1, prototype_args/1]).

-type kalerl_lineno() :: integer().
-type kalerl_variable() :: {variable, kalerl_lineno(), string()}.

%% Base type for all expression nodes. 
-type kalerl_expr() ::  
  %% variant for numeric literals like "1.0".
    {number, kalerl_lineno(), float()}
  %% variant for referencing a variable, like "a".
  | kalerl_variable()
  %% bindings
  | {'let', kalerl_lineno(), atom(), kalerl_expr(), [kalerl_expr()]}
  %% variant for a binary operator. *)
  | {binary, kalerl_lineno(), atom(), kalerl_expr(), kalerl_expr()}
  %% variant for a unary operator. *)
  | {unary, kalerl_lineno(), atom(), kalerl_expr()}
  %% control flow
  | {'if', kalerl_lineno(), kalerl_expr(), [kalerl_expr()], [kalerl_expr()]}
  %% loop
  | {for, kalerl_lineno(), string(), kalerl_expr(), kalerl_expr(), kalerl_expr(), [kalerl_expr()]}
  %% variant for function calls. *)
  | {call, kalerl_lineno(), string(), [kalerl_expr()]}.

%% Function prototype. Name and arguments
-type kalerl_proto() :: {prototype, kalerl_lineno(), string(), [kalerl_variable()]}
  | {binop_prototype, kalerl_lineno(), atom(), integer(), left | right, [kalerl_variable()]}
  | {unary_prototype, kalerl_lineno(), atom(), [kalerl_variable()]}.

%% Function definition
-type kalerl_func() :: {function, kalerl_lineno(), kalerl_proto(), [kalerl_expr()], atom() | none}.

-type kalerl_module() :: {module, kalerl_lineno(), string(), [kalerl_func()]}.
-export_type([kalerl_expr/0, kalerl_proto/0, kalerl_func/0, kalerl_module/0, kalerl_lineno/0]).

prototype_name({prototype, _Line, Name, _FormalArgs}) ->
  Name;
prototype_name({binop_prototype, _Line, OpID, _Precedence, _Association, _FormalArgs}) ->
  atom_to_list(OpID);
prototype_name({unary_prototype, _Line, OpID, _FormalArgs}) ->
  atom_to_list(OpID).

prototype_args({prototype, _Line, _Name, FormalArgs}) ->
  FormalArgs;
prototype_args({binop_prototype, _Line, _OpID, _Precedence, _Association, FormalArgs}) ->
  FormalArgs;
prototype_args({unary_prototype, _Line, _OpID, FormalArgs}) ->
  FormalArgs.
