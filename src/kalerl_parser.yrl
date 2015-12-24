Nonterminals 
expression primary_expr number_expr paren_expr identifier_expr identifier_list
bin_op_rhs bin_op_rhs_list prototype definition external toplevel_list toplevel
argument_expr_list if_expr for_expr for_step operator_precedence.

Terminals '(' ')' ',' '=' 'if' else then for in
operator def extern ident number binary.

Rootsymbol toplevel_list.

toplevel_list -> toplevel                 : '$1'.
toplevel_list -> toplevel toplevel_list   : toplevel_merge('$1', '$2').

toplevel -> expression                    : {toplevel, [], ['$1']}.
toplevel -> definition                    : {toplevel, ['$1'], []}.
toplevel -> external                      : {toplevel, ['$1'], []}.

expression -> primary_expr                : '$1'.
expression -> primary_expr bin_op_rhs_list : binop_finalize(binop_shunt(binop_push_expr('$1', '$2'))).

primary_expr -> identifier_expr             : '$1'.
primary_expr -> number_expr                 : '$1'.
primary_expr -> paren_expr                  : '$1'.
primary_expr -> if_expr                     : '$1'.
primary_expr -> for_expr                    : '$1'.

number_expr -> number                       : {number, line('$1'), unwrap('$1')}.

paren_expr -> '(' expression ')'            : '$1'.

identifier_expr -> ident                    : {variable, line('$1'), unwrap('$1')}.
identifier_expr -> ident '(' argument_expr_list ')' : {call, line('$1'), unwrap('$1'), '$3'}.

argument_expr_list -> expression            : ['$1'].
argument_expr_list -> expression ',' argument_expr_list : ['$1' | '$3'].
argument_expr_list -> '$empty'              : [].

if_expr -> 'if' expression then expression else expression : {'if', line('$1'), '$2', ['$4'], ['$6']}.

for_expr -> for ident '=' expression ',' expression for_step in expression : {for, line('$1'), unwrap('$2'), '$4', '$6', '$7', ['$9']}.
for_step -> '$empty'                        : {number, 0, 1.0}.
for_step -> ',' expression                  : '$2'.

bin_op_rhs -> operator primary_expr         : {unwrap('$1'), line('$1'), '$2'}.
bin_op_rhs_list -> bin_op_rhs               : binop_push_op('$1', {[], []}).
bin_op_rhs_list -> bin_op_rhs bin_op_rhs_list : binop_shunt(binop_push_op('$1', '$2')).

prototype -> ident '(' identifier_list ')'  : {prototype, line('$1'), unwrap('$1'), '$3'}.
prototype -> binary operator operator_precedence '(' identifier_list ')' : binop_register(unwrap('$2'), '$3'), {binop_prototype, line('$1'), unwrap('$2'), '$3', left, '$5'}.

operator_precedence -> '$empty'             : 30.
operator_precedence -> number               : trunc(unwrap('$1')).

identifier_list -> ident identifier_list    : [{variable, line('$1'), unwrap('$1')} | '$2'].
identifier_list -> '$empty'                 : [].

definition -> def prototype expression      : {function, line('$1'), '$2', ['$3'], none}.

external -> extern prototype in ident       : {function, line('$1'), '$2', [], list_to_atom(unwrap('$4'))}.

Erlang code.

-export([toplevel_to_module/2]).

unwrap({_,_,V}) -> V.

line({_,Line}) -> Line;
line({_,Line,_}) -> Line.

-type binop() :: atom().
-type shunt_data() :: {[{binop(), integer()}], [kalerl_ast:kalerl_expr()]}.

-spec binop_push_op({binop(), integer(), kalerl_ast:kalerl_expr()}, shunt_data()) -> shunt_data().
binop_push_op({Op, Line, Expr}, {Ops, Exprs}) ->
  {[{Op, Line} | Ops], [Expr | Exprs]}.

-spec binop_push_expr(kalerl_ast:kalerl_expr(), shunt_data()) -> shunt_data().
binop_push_expr(Expr, {Ops, Exprs}) ->
  {Ops, [Expr | Exprs]}.

-spec binop_pop_expr(shunt_data()) -> kalerl_ast:kalerl_expr().
binop_pop_expr({_Ops, [Expr | _Exprs]}) ->
  Expr.

-spec binop_shunt(shunt_data()) -> shunt_data().
binop_shunt(Data = {Ops, _OutputExprs}) when length(Ops) < 2 ->
  %% If only one operator, nothing to do
  Data;
binop_shunt(Data = {[{Op1, _Line1} | [{Op2, _Line2} | _RestOps]], _OutputExprs}) ->
  %% Shunting-yard algorithm, kinda
  case binop_should_shunt(Op1, Op2) of
    true -> binop_shunt(binop_do_shunt(Data));
    false -> Data
  end.

-spec binop_do_shunt(shunt_data()) -> shunt_data().
binop_do_shunt({[Op1 | [{Op2, Line2} | RestOps]], [Expr1 | [Expr2 | RestExprs]]}) ->
  NewExpr = {binary, Line2, Op2, Expr1, Expr2},
  NewData = {[Op1 | RestOps], [NewExpr | RestExprs]},
  NewData.
  
-spec binop_should_shunt(binop(), binop()) -> boolean().
binop_should_shunt(Op1, Op2) ->
  Op1Associate = binop_associate(Op1),
  Op1Precedence = binop_precedence(Op1),
  Op2Precedence = binop_precedence(Op2),
  ((Op1Associate =:= left) and (Op1Precedence =< Op2Precedence)) or ((Op1Associate =:= right) and (Op1Precedence < Op2Precedence)).
    
-spec binop_finalize(shunt_data()) -> kalerl_ast:kalerl_expr().
binop_finalize(Data = {Ops, _Exprs}) when length(Ops) =:= 0 ->
  binop_pop_expr(Data);
binop_finalize({[{Op, Line} | RestOps], [Expr1 | [Expr2 | RestExprs]]}) ->
  %% Pop first operator off and combine it with the top two expressions
  NewExpr = {binary, Line, Op, Expr1, Expr2},
  NewData = {RestOps, [NewExpr | RestExprs]},
  binop_finalize(NewData).

-spec binop_precedence(binop()) -> integer().
binop_precedence(Op) ->
  kalerl_optable:precedence(Op, self()).

-spec binop_associate(binop()) -> left | right.
binop_associate(Op) -> 
  kalerl_optable:association(Op, self()).

-spec binop_register(binop(), integer()) -> ok.
binop_register(Op, Precedence) ->
  kalerl_optable:add_operator(Op, Precedence, left, self()).

-type toplevel() :: {toplevel, [kalerl_ast:kalerl_func()], [kalerl_ast:kalerl_expr()]}.
-spec toplevel_merge(toplevel(), toplevel()) -> toplevel().
toplevel_merge({toplevel, NewFuncs, NewExprs}, {toplevel, ExistingFuncs, ExistingExprs}) ->
  {toplevel, ExistingFuncs ++ NewFuncs, ExistingExprs ++ NewExprs}.
  
-spec toplevel_to_module(toplevel(), string()) -> {ok, kalerl_ast:kalerl_module()}.
toplevel_to_module({toplevel, Funcs, MainExprs}, ModuleName) ->
  Main = {function, 1, {prototype, 1, "main", []}, main_exprs(MainExprs), none},
  {ok, {module, 1, ModuleName, Funcs ++ [Main]}}.

-spec main_exprs([kalerl_ast:kalerl_expr()]) -> [kalerl_ast:kalerl_expr()].
main_exprs([]) ->
  [{number, 1, 1.0}];
main_exprs(MainExprs) ->
  MainExprs.

