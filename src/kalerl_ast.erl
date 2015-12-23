%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_ast).

-type kalerl_lineno() :: integer().
-type kalerl_variable() :: {variable, kalerl_lineno(), string()}.

%% Base type for all expression nodes. 
-type kalerl_expr() ::  
  %% variant for numeric literals like "1.0".
    {number, kalerl_lineno(), float()}
  %% variant for referencing a variable, like "a".
  | kalerl_variable()
  %% variant for a binary operator. *)
  | {binary, kalerl_lineno(), atom(), kalerl_expr(), kalerl_expr()}
  %% control flow
  | {'if', kalerl_lineno(), kalerl_expr(), [kalerl_expr()], [kalerl_expr()]}
  %% variant for function calls. *)
  | {call, kalerl_lineno(), string(), [kalerl_expr()]}.
  
%% Function prototype. Name and arguments
-type kalerl_proto() :: {prototype, kalerl_lineno(), string(), [kalerl_variable()]}.

%% Function definition
-type kalerl_func() :: {function, kalerl_lineno(), kalerl_proto(), [kalerl_expr()]}.

-type kalerl_module() :: {module, kalerl_lineno(), string(), [kalerl_func() | kalerl_proto()]}.
-export_type([kalerl_expr/0, kalerl_proto/0, kalerl_func/0, kalerl_module/0, kalerl_lineno/0]).
