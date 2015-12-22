%% @author Andrew J. Finnell
%% @copyright 2015 Andrew J. Finnell.

-module(kalerl_binarygen).

-export([forms/1, format_error/1]).

%% Public API

forms(AbsForms) ->
  process_binary(compile:forms(AbsForms, [verbose, return, export_all])).

format_error(unknown) ->
  "Unknown compile error".

%% Implementation

process_binary({ok,ModuleName,BinaryOrCode}) ->
  {ok, ModuleName, BinaryOrCode, []};
process_binary(Ret = {ok, _ModuleName, _BinaryOrCode, _Warnings}) ->
  Ret;
process_binary(error) ->
  {error, {[{"<<internal>>", [{none, ?MODULE, unknown}]}], []}};
process_binary({error, Errors, Warnings}) ->
  {error, {Errors, Warnings}}.
