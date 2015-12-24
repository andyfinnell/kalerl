%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_optable).
-behaviour(gen_server).

-export([start_link/0, add_operator/4, precedence/2, association/2, is_builtin/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(op, {precedence :: integer(),
  association :: left | right
}).
-record(optable, {tables :: map()}).

%% Public API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).	

add_operator(Op, Precedence, Association, Pid) ->
  gen_server:call(?MODULE, {add, Op, Precedence, Association, Pid}).
  
precedence(Op, Pid) ->
  gen_server:call(?MODULE, {precedence, Op, Pid}).
  
association(Op, Pid) ->
  gen_server:call(?MODULE, {association, Op, Pid}).

is_builtin(Op) ->
  %% Ask for a client we know doesn't exist so we get the defaults
  precedence(Op, none) =/= none.

%% gen_server Callbacks
	
init([]) ->
  State = #optable{tables = #{}},
  {ok, State}.

handle_call({add, Op, Precedence, Association, Pid}, _From, State = #optable{tables = Tables}) ->
  OpRecord = #op{precedence = Precedence, association = Association},
  NewState = case maps:find(Pid, Tables) of
    {ok, Table} ->
      State#optable{tables = Tables#{ Pid => Table#{Op => OpRecord} } };
    error ->
      State#optable{tables = Tables#{ Pid => #{Op => OpRecord} } }
  end,
  {reply, ok, NewState};
handle_call({precedence, Op, Pid}, _From, State) ->
  Reply = case oprecord(Op, Pid, State) of
    none -> none;
    OpRecord -> OpRecord#op.precedence
  end,
  {reply, Reply, State};
handle_call({association, Op, Pid}, _From, State) ->
  Reply = case oprecord(Op, Pid, State) of
    none -> none;
    OpRecord -> OpRecord#op.association
  end,
  {reply, Reply, State};
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Implementation

oprecord('<', _Pid, _State) -> #op{precedence = 10, association = left};
oprecord('+', _Pid, _State) -> #op{precedence = 20, association = left};
oprecord('-', _Pid, _State) -> #op{precedence = 20, association = left};
oprecord('*', _Pid, _State) -> #op{precedence = 40, association = left};
oprecord(Op, Pid, #optable{tables = Tables}) ->
  case maps:find(Pid, Tables) of
    {ok, Table} ->
      case maps:find(Op, Table) of
        {ok, OpRecord} -> OpRecord;
        error -> none
      end;
    error -> none
  end.
