Nonterminals 
expression primary_expr number_expr paren_expr identifier_expr identifier_list
bin_op_rhs bin_op_rhs_list prototype definition external toplevel_list toplevel
argument_expr_list.

Terminals '(' ')' ','
operator def extern ident number.

Rootsymbol toplevel_list.

toplevel_list -> toplevel                 : ['$1'].
toplevel_list -> toplevel toplevel_list   : ['$1' | '$2'].

toplevel -> expression                    : '$1'.
toplevel -> definition                    : '$1'.
toplevel -> external                      : '$1'.

expression -> primary_expr                : '$1'.
expression -> primary_expr bin_op_rhs_list : binop_finalize(binop_shunt(binop_push_expr('$1', '$2'))).

primary_expr -> identifier_expr             : '$1'.
primary_expr -> number_expr                 : '$1'.
primary_expr -> paren_expr                  : '$1'.

number_expr -> number                       : {number, unwrap('$1')}.

paren_expr -> '(' expression ')'            : '$1'.

identifier_expr -> ident                    : {variable, unwrap('$1')}.
identifier_expr -> ident '(' argument_expr_list ')' : {call, unwrap('$1'), '$3'}.

argument_expr_list -> expression            : ['$1'].
argument_expr_list -> expression ',' argument_expr_list : ['$1' | '$3'].
argument_expr_list -> '$empty'              : [].

bin_op_rhs -> operator primary_expr         : {unwrap('$1'), '$2'}.
bin_op_rhs_list -> bin_op_rhs               : binop_push_op('$1', {[], []}).
bin_op_rhs_list -> bin_op_rhs bin_op_rhs_list : binop_shunt(binop_push_op('$1', '$2')).

prototype -> ident '(' identifier_list ')'  : {prototype, unwrap('$1'), '$3'}.

identifier_list -> ident identifier_list    : [unwrap('$1') | '$2'].
identifier_list -> '$empty'                 : [].

definition -> def prototype expression      : {function, '$2', '$3'}.

external -> extern prototype                : '$2'.

Erlang code.

unwrap({_,_,V}) -> V.

-type binop() :: atom().
-type shunt_data() :: {[binop()], [kalerl_ast:kalerl_expr()]}.

-spec binop_push_op({binop(), kalerl_ast:kalerl_expr()}, shunt_data()) -> shunt_data().
binop_push_op({Op, Expr}, {Ops, Exprs}) ->
  {[Op | Ops], [Expr | Exprs]}.

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
binop_shunt(Data = {[Op1 | [Op2 | _RestOps]], _OutputExprs}) ->
  %% Shunting-yard algorithm, kinda
  case binop_should_shunt(Op1, Op2) of
    true -> binop_shunt(binop_do_shunt(Data));
    false -> Data
  end.

-spec binop_do_shunt(shunt_data()) -> shunt_data().
binop_do_shunt({[Op1 | [Op2 | RestOps]], [Expr1 | [Expr2 | RestExprs]]}) ->
  NewExpr = {binary, Op2, Expr1, Expr2},
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
binop_finalize({[Op | RestOps], [Expr1 | [Expr2 | RestExprs]]}) ->
  %% Pop first operator off and combine it with the top two expressions
  NewExpr = {binary, Op, Expr1, Expr2},
  NewData = {RestOps, [NewExpr | RestExprs]},
  binop_finalize(NewData).

-spec binop_precedence(binop()) -> integer().
binop_precedence('<') -> 10;
binop_precedence('+') -> 20;
binop_precedence('-') -> 20;
binop_precedence('*') -> 40.

-spec binop_associate(binop()) -> left | right.
binop_associate(_Op) -> left.
