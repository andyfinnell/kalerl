-module(kalstd).

-export([printfloat/1]).

printfloat(F) ->
  io:format("~p~n", [F]).
