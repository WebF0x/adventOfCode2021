-module(advent).

-export([
  day1/0,
  get_differences/1
]).

-include_lib("eunit/include/eunit.hrl").

day1() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  Depths = [binary_to_integer(DepthString) || DepthString <- Lines],
  Differences = get_differences(Depths),
  Increases = [X || X <- Differences, X =:= increased],
  IncreasedCount = length(Increases),
  io:fwrite("~p~n", [IncreasedCount]),
  ok.

get_differences([_]) ->
  [na];
get_differences([_Head | _Tail] = Depths) ->
  Heads = lists:droplast(Depths),
  Last = lists:last(Depths),
  SecondToLast = lists:last(Heads),
  Diff = case (Last - SecondToLast > 0) of
           true -> increased;
           false -> not_increased
         end,
  get_differences(Heads) ++ [Diff].

day1_test() ->
  ?assertMatch([na], get_differences([1])),
  ?assertMatch([na, increased], get_differences([1, 2])),
  ?assertMatch([na, increased, increased], get_differences([1, 2, 3])),
  ?assertMatch([na, not_increased, not_increased], get_differences([3, 2, 1])),
  ?assertMatch([na, not_increased, not_increased], get_differences([3, 3, 3])),
  ok.


% read file
% day1
% count nb of increased