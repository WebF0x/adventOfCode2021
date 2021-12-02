-module(advent).

-export([
  day1_part1/0,
  day1_part2/0,
  day2_part1/0,
  day2_part2/0,
  get_differences/1,
  get_trio_sums/1
]).

-include_lib("eunit/include/eunit.hrl").

%%###############%%
%%     DAY 1     %%
%%###############%%

day1_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  Depths = [binary_to_integer(DepthString) || DepthString <- Lines],
  Differences = get_differences(Depths),
  Increases = [X || X <- Differences, X =:= increased],
  IncreasedCount = length(Increases),
  io:fwrite("~p~n", [IncreasedCount]),
  ok.

day1_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  Depths = [binary_to_integer(DepthString) || DepthString <- Lines],
  Trios = get_trio_sums(Depths),
  Differences = get_differences(Trios),
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

get_trio_sums([_A]) ->
  [];
get_trio_sums([_A, _B]) ->
  [];
get_trio_sums([A, B, C | Tail]) ->
  Sum = A + B + C,
  [Sum] ++ get_trio_sums([B, C] ++ Tail).

day1_test() ->
  ?assertMatch([na], get_differences([1])),
  ?assertMatch([na, increased], get_differences([1, 2])),
  ?assertMatch([na, increased, increased], get_differences([1, 2, 3])),
  ?assertMatch([na, not_increased, not_increased], get_differences([3, 2, 1])),
  ?assertMatch([na, not_increased, not_increased], get_differences([3, 3, 3])),

  ?assertMatch([], get_trio_sums([1])),
  ?assertMatch([], get_trio_sums([1, 2])),
  ?assertMatch([6], get_trio_sums([1, 2, 3])),
  ?assertMatch([6, 9], get_trio_sums([1, 2, 3, 4])),
  ok.

%%###############%%
%%     DAY 2     %%
%%###############%%

day2_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  SplitLines = [string:lexemes(Line, " ") || Line <- Lines],
  Commands = [{list_to_atom(binary_to_list(Command)), binary_to_integer(Distance)} || [Command, Distance] <- SplitLines],
  Position = move_submarine(Commands),
  {X, Y} = Position,
  io:fwrite("~p~n", [Position]),
  io:fwrite("~p~n", [X * Y]),
  ok.

day2_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  SplitLines = [string:lexemes(Line, " ") || Line <- Lines],
  Commands = [{list_to_atom(binary_to_list(Command)), binary_to_integer(Distance)} || [Command, Distance] <- SplitLines],
  Position = move_submarine_with_aim(Commands),
  {X, Y} = Position,
  io:fwrite("~p~n", [Position]),
  io:fwrite("~p~n", [X * Y]),
  ok.

move_submarine([Command | RemainingCommands]) ->
  {DeltaX, DeltaY} = case Command of
    {up, Distance} -> {0, -Distance};
    {forward, Distance} -> {Distance, 0};
    {down, Distance} -> {0, Distance}
  end,
  {X, Y} = move_submarine(RemainingCommands),
  {X + DeltaX, Y + DeltaY};
move_submarine([]) ->
  {0, 0}.


move_submarine_with_aim(Commands) ->
  move_submarine_with_aim(Commands, 0).
move_submarine_with_aim([Command | RemainingCommands], Aim) ->
  {DeltaX, DeltaY, NewAim} = case Command of
    {up, Distance} -> {0, 0, Aim-Distance};
    {forward, Distance} -> {Distance, Aim*Distance, Aim};
    {down, Distance} -> {0, 0, Aim+Distance}
  end,
  {X, Y} = move_submarine_with_aim(RemainingCommands, NewAim),
  {X + DeltaX, Y + DeltaY};
move_submarine_with_aim([], _) ->
  {0, 0}.

day2_test() ->
  ?assertMatch({0, 0}, move_submarine([])),
  ?assertMatch({1, 0}, move_submarine([{forward, 1}])),
  ?assertMatch({0, -2}, move_submarine([{up, 2}])),
  ?assertMatch({0, 3}, move_submarine([{down, 3}])),
  ?assertMatch({15, 10}, move_submarine([{forward, 5}, {down, 5}, {forward, 8}, {up, 3}, {down, 8}, {forward, 2}])),

  ?assertMatch({0, 0}, move_submarine_with_aim([])),
  ?assertMatch({1, 0}, move_submarine_with_aim([{forward, 1}])),
  ?assertMatch({2, 6}, move_submarine_with_aim([{down, 3}, {forward, 2}])),
  ?assertMatch({15, 60}, move_submarine_with_aim([{forward, 5}, {down, 5}, {forward, 8}, {up, 3}, {down, 8}, {forward, 2}])),
  ok.
