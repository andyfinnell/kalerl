%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_ast).

%% Base type for all expression nodes. 
-type kalerl_expr() ::  
  %% variant for numeric literals like "1.0".
    {number, float()}
  %% variant for referencing a variable, like "a".
  | {variable, string()}
  %% variant for a binary operator. *)
  | {binary, atom(), kalerl_expr(), kalerl_expr()}
  %% variant for function calls. *)
  | {call, string(), [kalerl_expr()]}.
  
%% Function prototype. Name and arguments
-type kalerl_proto() :: {prototype, string(), [string()]}.

%% Function definition
-type kalerl_func() :: {function, kalerl_proto(), kalerl_expr()}.

-export_type([kalerl_expr/0, kalerl_proto/0, kalerl_func/0]).
