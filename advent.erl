-module(advent).

-export([day1/0]).

-include_lib("eunit/include/eunit.hrl").

day1() ->
  io:fwrite("Hello, world!\n").

day1_test() ->
  ?assertMatch(a, b).
