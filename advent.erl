-module(advent).

-export([
  day1_part1/0,
  day1_part2/0,
  day2_part1/0,
  day2_part2/0,
  day3_part1/0,
  day3_part2/0,
  day4_part1/0,
  day4_part2/0,
  day5_part2/0,
  day6_part1/0,
  day6_part2/0,
  day7_part1/0,
  day7_part2/0,
  day8_part1/0,
  day8_part2/0,
  day9_part1/0,
  day9_part2/0,
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

%%###############%%
%%     DAY 3     %%
%%###############%%

day3_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  DiagnosticReport = [binary_to_list(Line) || Line <- Lines],
  io:fwrite("~p~n", [DiagnosticReport]),
  GammaDecimal = decimal(gamma(DiagnosticReport)),
  EpsilonDecimal = decimal(epsilon(DiagnosticReport)),
  io:fwrite("~p~n", [GammaDecimal * EpsilonDecimal]),
  ok.

day3_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  Lines = string:lexemes(Content, "\n"),
  DiagnosticReport = [binary_to_list(Line) || Line <- Lines],
  OxygenRating = decimal(oxygen(DiagnosticReport)),
  Co2Rating = decimal(co2(DiagnosticReport)),
  io:fwrite("~p~n", [OxygenRating * Co2Rating]),
  ok.

day3_test() ->
  DiagnosticReport = [
    "001",
    "111",
    "101"
  ],
  ?assertMatch([2, 1, 3], counts(DiagnosticReport)),
  ?assertMatch([1, 0, 1], gamma(DiagnosticReport)),
  ?assertMatch([0, 1, 0], epsilon(DiagnosticReport)),
  ?assertMatch(5, decimal([1, 0, 1])),

  DiagnosticReport2 = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ],
  FilteredReport = [
    "11110",
    "10110",
    "10111",
    "10101",
    "11100",
    "10000",
    "11001"
  ],
  ?assertMatch(FilteredReport, filter(DiagnosticReport2, [1])),
  ?assertMatch([1,0,1,1,1], oxygen(DiagnosticReport2)),
  ?assertMatch([0,1,0,1,0], co2(DiagnosticReport2)),
  ?assertMatch(23, decimal([1, 0, 1, 1, 1])),
  ?assertMatch(10, decimal([0, 1, 0, 1, 0])),
  ok.

counts([Head | _Tail] = DiagnosticReport) ->
  Counts = [0 || _ <- Head],
  counts(DiagnosticReport, Counts).

counts([], Counts) ->
  Counts;
counts([Head | Tail], Counts) ->
  CountsSoFar = lists:zipwith(
    fun(Char, Value) ->
      case Char of
        49 -> Value + 1;
        48 -> Value
      end
    end,
    Head,
    Counts
  ),
  counts(Tail, CountsSoFar).

gamma(DiagnosticReport) ->
  Counts = counts(DiagnosticReport),
  Threshold = length(DiagnosticReport) / 2,
  [
    case X >= Threshold of
      true -> 1;
      false -> 0
    end || X <- Counts
  ].

epsilon(DiagnosticReport) ->
  Gamma = gamma(DiagnosticReport),
  [
    case X of
      1 -> 0;
      0 -> 1
    end || X <- Gamma
  ].

decimal([]) ->
  0;
decimal([Head | Tail] = Bits) ->
  FloatValue = Head * math:pow(2, length(Bits) - 1),
  Value = list_to_integer(float_to_list(FloatValue, [{decimals, 0}])),
  Value + decimal(Tail).

oxygen(DiagnosticReport) ->
  oxygen(DiagnosticReport, []).

oxygen([Rating], _) ->
  lists:map(fun(Character) -> 
    case Character of
      49 -> 1;
      48 -> 0
    end
  end, Rating);
oxygen(DiagnosticReport, Bits) ->
  NewBit = lists:nth(length(Bits) + 1, gamma(DiagnosticReport)),
  NewBits = Bits ++ [NewBit],
  RemainingDiagnotic = filter(DiagnosticReport, NewBits),
  oxygen(RemainingDiagnotic, NewBits).

co2(DiagnosticReport) ->
  co2(DiagnosticReport, []).

co2([Rating], _) ->
  lists:map(fun(Character) -> 
    case Character of
      49 -> 1;
      48 -> 0
    end
  end, Rating);
co2(DiagnosticReport, Bits) ->
  NewBit = lists:nth(length(Bits) + 1, epsilon(DiagnosticReport)),
  NewBits = Bits ++ [NewBit],
  RemainingDiagnotic = filter(DiagnosticReport, NewBits),
  co2(RemainingDiagnotic, NewBits).

filter(DiagnosticReport, Bits) ->
  FilterBit = case lists:last(Bits) of
    1 -> 49;
    0 -> 48
  end,
  Index = length(Bits),
  NewDiagnosticReport = [DiagnosticData || DiagnosticData <- DiagnosticReport, lists:nth(Index, DiagnosticData) == FilterBit],
  if length(NewDiagnosticReport) == 0 ->
    [lists:last(DiagnosticReport)];
  true -> 
    NewDiagnosticReport
  end.

%%###############%%
%%     DAY 4     %%
%%###############%%

day4_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  [NumbersString | BoardsString] = string:split(Content, "\n\n", all),
  BoardsRaw = [string:lexemes(BoardString, "\n") || BoardString <- BoardsString],
  Boards = lists:map(
  fun(Board) -> 
    lists:map(
      fun(BoardRow) -> 
        Numbers = string:lexemes(BoardRow, " "),
        [{binary_to_integer(Number), unmarked} || Number <- Numbers]
      end, Board)
  end, BoardsRaw),
  Numbers = [binary_to_integer(Number) || Number <- string:lexemes(NumbersString, ",")],
  {Index, WinningBoard} = winning_board(Numbers, Boards),
  BoardScore = board_score(WinningBoard),
  {WinningMove, _} = winning_move(Numbers, lists:nth(Index, Boards)),
  WinningNumber = lists:nth(WinningMove, Numbers),
  io:fwrite("Score: ~p~n", [BoardScore]),
  io:fwrite("Winning Number: ~p~n", [WinningNumber]),
  io:fwrite("~p~n", [BoardScore * WinningNumber]),
  ok.

day4_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  [NumbersString | BoardsString] = string:split(Content, "\n\n", all),
  BoardsRaw = [string:lexemes(BoardString, "\n") || BoardString <- BoardsString],
  Boards = lists:map(
  fun(Board) -> 
    lists:map(
      fun(BoardRow) -> 
        Numbers = string:lexemes(BoardRow, " "),
        [{binary_to_integer(Number), unmarked} || Number <- Numbers]
      end, Board)
  end, BoardsRaw),
  Numbers = [binary_to_integer(Number) || Number <- string:lexemes(NumbersString, ",")],
  {Index, LosingBoard} = losing_board(Numbers, Boards),
  BoardScore = board_score(LosingBoard),
  {WinningMove, _} = winning_move(Numbers, lists:nth(Index, Boards)),
  WinningNumber = lists:nth(WinningMove, Numbers),
  io:fwrite("Score: ~p~n", [BoardScore]),
  io:fwrite("Winning Number: ~p~n", [WinningNumber]),
  io:fwrite("~p~n", [BoardScore * WinningNumber]),
  ok.

day4_test() ->
  Numbers = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
  Boards = [
    [[{22, unmarked}, {13, unmarked}, {17, unmarked}, {11, unmarked},  {0, unmarked}],
    [{8, unmarked},  {2, unmarked}, {23, unmarked},  {4, unmarked}, {24, unmarked}],
    [{21, unmarked},  {9, unmarked}, {14, unmarked}, {16, unmarked},  {7, unmarked}],
    [{6, unmarked}, {10, unmarked},  {3, unmarked}, {18, unmarked},  {5, unmarked}],
    [{1, unmarked}, {12, unmarked}, {20, unmarked}, {15, unmarked}, {19, unmarked}]],
    [[{3, unmarked}, {15, unmarked},  {0, unmarked},  {2, unmarked}, {22, unmarked}],
    [{9, unmarked}, {18, unmarked}, {13, unmarked}, {17, unmarked},  {5, unmarked}],
    [{19, unmarked},  {8, unmarked},  {7, unmarked}, {25, unmarked}, {23, unmarked}],
    [{20, unmarked}, {11, unmarked}, {10, unmarked}, {24, unmarked},  {4, unmarked}],
    [{14, unmarked}, {21, unmarked}, {16, unmarked}, {12, unmarked},  {6, unmarked}]],
    [[{14, unmarked}, {21, unmarked}, {17, unmarked}, {24, unmarked},  {4, unmarked}],
    [{10, unmarked}, {16, unmarked}, {15, unmarked},  {9, unmarked}, {19, unmarked}],
    [{18, unmarked},  {8, unmarked}, {23, unmarked}, {26, unmarked}, {20, unmarked}],
    [{22, unmarked}, {11, unmarked}, {13, unmarked},  {6, unmarked},  {5, unmarked}],
    [{2, unmarked},  {0, unmarked}, {12, unmarked},  {3, unmarked},  {7, unmarked}]]
  ],
  WinningBoard = [
    [{14, marked}, {21, marked}, {17, marked}, {24, marked},  {4, marked}],
    [{10, unmarked}, {16, unmarked}, {15, unmarked},  {9, marked}, {19, unmarked}],
    [{18, unmarked},  {8, unmarked}, {23, marked}, {26, unmarked}, {20, unmarked}],
    [{22, unmarked}, {11, marked}, {13, unmarked},  {6, unmarked},  {5, marked}],
    [{2, marked},  {0, marked}, {12, unmarked},  {3, unmarked},  {7, marked}]
  ],

  ?assertMatch(188, board_score(WinningBoard)),
  ?assertMatch({12, WinningBoard}, winning_move(Numbers, lists:nth(3, Boards))),
  ?assertMatch({3, WinningBoard}, winning_board(Numbers, Boards)),
  ok.

board_score(Board) ->
  ScoreNumbers = lists:flatmap(
    fun(BoardRow) -> 
      [Number || {Number, State} <- BoardRow, State == unmarked] 
    end, Board),
  lists:sum(ScoreNumbers).

winning_move(Numbers, Board) ->
  winning_move(Numbers, Board, 1).

winning_move([HeadNumber | TailNumbers], Board, Moves) ->
  NewBoard = mark_number(HeadNumber, Board),
  case is_board_winning(NewBoard) of
    true -> {Moves, NewBoard};
    false -> winning_move(TailNumbers, NewBoard, Moves + 1)
  end.

mark_number(Number, Board) ->
  lists:map(
    fun(BoardRow) ->
      lists:map(
        fun({BoardNumber, _} = BoardCell) ->
          if 
            BoardNumber == Number ->
              {BoardNumber, marked};
            true ->
              BoardCell
          end
        end, BoardRow)
    end, Board).

is_board_winning(Board) ->
  WinningRows = winning_rows(Board),
  WinningColumns = winning_rows(transpose(Board)),
  (length(WinningColumns) =/= 0 orelse length(WinningRows) =/= 0).

winning_rows(Board) ->
  lists:filter(fun(BoardRow) ->
    UnmarkedCells = [marked || {_, State} <- BoardRow, State == unmarked],
    if 
      length(UnmarkedCells) == 0 ->
        true;
      true ->
        false
    end
  end, Board).

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

winning_board(Numbers, Boards) ->
  WinningMoves = lists:map(fun(Board) ->
    {Move, _} = winning_move(Numbers, Board),
    Move
  end, Boards),
  WinningMove = lists:min(WinningMoves),
  Index = index_of(WinningMove, WinningMoves),
  {_, WinningBoard} = winning_move(Numbers, lists:nth(Index, Boards)),
  {Index, WinningBoard}.

losing_board(Numbers, Boards) ->
  WinningMoves = lists:map(fun(Board) ->
    {Move, _} = winning_move(Numbers, Board),
    Move
                           end, Boards),
  LosingMove = lists:max(WinningMoves),
  Index = index_of(LosingMove, WinningMoves),
  {_, LosingBoard} = winning_move(Numbers, lists:nth(Index, Boards)),
  {Index, LosingBoard}.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).

%%###############%%
%%     DAY 5     %%
%%###############%%

day5_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  RawVentsLines = string:lexemes(Content, "\n"),
  VentsLines = lists:map(
    fun(RawVentsLine) ->
      [From, To] = string:split(RawVentsLine, " -> ", all),
      [X1, Y1] = string:split(From, ",", all),
      [X2, Y2] = string:split(To, ",", all),
      {{from, binary_to_integer(X1), binary_to_integer(Y1)}, {to, binary_to_integer(X2), binary_to_integer(Y2)}}
    end,
    RawVentsLines
  ),
  RowWithZeroes = lists:duplicate(1000, 0),
  Map = lists:duplicate(1000, RowWithZeroes),
  VentsMap = lists:flatten(place_vents(Map, VentsLines)),
  Answer = length(lists:filter(fun(NbVent) -> NbVent >= 2 end, VentsMap)),

  io:fwrite("Answer: ~p~n", [Answer]),
  ok.

place_vents(OriginalMap, VentsLines) ->
  Vents = lists:flatmap(fun line_to_points/1, VentsLines),

  lists:foldl(
    fun({X, Y}, Map) ->
      {PreRow, [Row | PostRow]} = lists:split(Y, Map),
      {PreNbVents, [NbVents | PostNbVents]} = lists:split(X, Row),
      NewRow = PreNbVents ++ [NbVents + 1] ++ PostNbVents,
      NewMap = PreRow ++ [NewRow] ++ PostRow,
      NewMap
    end,
    OriginalMap,
    Vents).

line_to_points({{from, X1, Y}, {to, X2, Y}}) ->
  Increment = if
                X2 > X1 -> 1;
                true -> -1
              end,
  XPositions = lists:seq(X1, X2, Increment),
  [{X, Y} || X <- XPositions];
line_to_points({{from, X, Y1}, {to, X, Y2}}) ->
  Increment = if
                Y2 > Y1 -> 1;
                true -> -1
              end,
  YPositions = lists:seq(Y1, Y2, Increment),
  [{X, Y} || Y <- YPositions];
line_to_points({{from, X1, Y1}, {to, X2, Y2}}) ->
  XIncrement = if
                 X2 > X1 -> 1;
                 true -> -1
               end,
  YIncrement = if
                 Y2 > Y1 -> 1;
                 true -> -1
               end,
  XPositions = lists:seq(X1, X2, XIncrement),
  YPositions = lists:seq(Y1, Y2, YIncrement),
  lists:zip(XPositions, YPositions).

day5_test() ->
  ?assertMatch([{0, 0}, {1, 0}, {2, 0}], line_to_points({{from, 0, 0}, {to, 2, 0}})),
  ?assertMatch([{0, 0}, {0, 1}, {0, 2}], line_to_points({{from, 0, 0}, {to, 0, 2}})),
  ?assertMatch([{0, 0}, {1, 1}, {2, 2}], line_to_points({{from, 0, 0}, {to, 2, 2}})),
  ?assertMatch([{2, 0}, {1, 1}, {0, 2}], line_to_points({{from, 2, 0}, {to, 0, 2}})),

  StartingMap = [
    [0, 0],
    [0, 0]
  ],
  MapShouldntChange = place_vents(StartingMap, []),
  ?assertMatch(StartingMap, MapShouldntChange),

  VentsLines = [{{from, 0, 0}, {to, 1, 0}}],
  ExpectedMap = [
    [1, 1],
    [0, 0]
  ],
  Map = place_vents(StartingMap, VentsLines),
  ?assertMatch(ExpectedMap, Map),
  ok.

%%###############%%
%%     DAY 6     %%
%%###############%%

day6_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  LanternfishStrings = string:lexemes(Content, ",\n"),
  Lanternfishes = [binary_to_integer(LanternfishString) || LanternfishString <- LanternfishStrings],
  NbFishes = lists:zip(lists:seq(0,8), lists:duplicate(9, 0)),
  LanternfishInternalTimers = lists:foldl(
    fun(Lanternfish, NbFishesAcc) ->
      {_, Value} = lists:keyfind(Lanternfish, 1, NbFishesAcc),
      lists:keyreplace(Lanternfish, 1, NbFishesAcc, {Lanternfish, Value + 1})
    end,
    NbFishes,
    Lanternfishes
  ),
  LanternfishCount = simulate_days(LanternfishInternalTimers, 80),
  io:fwrite("Answer: ~p~n", [LanternfishCount]),
  ok.

day6_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  LanternfishStrings = string:lexemes(Content, ",\n"),
  Lanternfishes = [binary_to_integer(LanternfishString) || LanternfishString <- LanternfishStrings],
  NbFishes = lists:zip(lists:seq(0,8), lists:duplicate(9, 0)),
  LanternfishInternalTimers = lists:foldl(
    fun(Lanternfish, NbFishesAcc) ->
      {_, Value} = lists:keyfind(Lanternfish, 1, NbFishesAcc),
      lists:keyreplace(Lanternfish, 1, NbFishesAcc, {Lanternfish, Value + 1})
    end,
    NbFishes,
    Lanternfishes
  ),
  LanternfishCount = simulate_days(LanternfishInternalTimers, 256),
  io:fwrite("Answer: ~p~n", [LanternfishCount]),
  ok.

simulate_day([{0, Value0}, {1, Value1}, {2, Value2}, {3, Value3}, {4, Value4}, {5, Value5}, {6, Value6}, {7, Value7}, {8, Value8}]) ->
  [{0, Value1}, {1, Value2}, {2, Value3}, {3, Value4}, {4, Value5}, {5, Value6}, {6, Value7+Value0}, {7, Value8}, {8, Value0}].

simulate_days(InternalTimers, 0) ->
  lists:sum([Value || {_, Value} <- InternalTimers]);
simulate_days(InternalTimers, Days) ->
  simulate_days(simulate_day(InternalTimers), Days-1).

day6_test() ->
  LanternfishInternalTimers = [{0, 0}, {1, 1}, {2, 1}, {3, 2}, {4, 1}, {5, 0}, {6, 0}, {7, 0}, {8, 0}],
  InternalTimersAfterOneDay = [{0, 1}, {1, 1}, {2, 2}, {3, 1}, {4, 0}, {5, 0}, {6, 0}, {7, 0}, {8, 0}],
  ExpectedCount = 5934,
  ExpectedCount2 = 26984457539,
  ?assertMatch(InternalTimersAfterOneDay, simulate_day(LanternfishInternalTimers)),
  ?assertMatch(ExpectedCount, simulate_days(LanternfishInternalTimers, 80)),
  ?assertMatch(ExpectedCount2, simulate_days(LanternfishInternalTimers, 256)),
  ok.

%%###############%%
%%     DAY 7     %%
%%###############%%

day7_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  CrabsRaw = string:lexemes(Content, ",\n"),
  Crabs = [binary_to_integer(CrabRaw) || CrabRaw <- CrabsRaw],
  Positions = lists:seq(0, lists:max(Crabs)),
  FuelCosts = lists:map(
    fun(Position) ->
      AbsDifferences = [abs(Crab - Position) || Crab <- Crabs],
      lists:sum(AbsDifferences)
    end,
    Positions
  ),
  io:fwrite("Answer: ~p~n", [lists:min(FuelCosts)]),
  ok.

day7_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  CrabsRaw = string:lexemes(Content, ",\n"),
  Crabs = [binary_to_integer(CrabRaw) || CrabRaw <- CrabsRaw],
  Positions = lists:seq(0, lists:max(Crabs)),
  FuelCosts = lists:map(
    fun(Position) ->
      AbsDifferences = [abs(Crab - Position) || Crab <- Crabs],
      FuelCost = [(N + 1) * N / 2 || N <- AbsDifferences],
      list_to_integer(float_to_list(lists:sum(FuelCost), [{decimals, 0}]))
    end,
    Positions
  ),
  io:fwrite("Answer: ~p~n", [lists:min(FuelCosts)]),
  ok.

%%###############%%
%%     DAY 8     %%
%%###############%%

day8_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  DisplaysRaw = string:lexemes(Content, "\n"),
  OutputValues = lists:map(
    fun(DisplayRaw) ->
      [_, Output] = string:split(DisplayRaw, " | ", all),
      Numbers = string:split(Output, " ", all),
      [decode_easy_number(binary_to_list(Number)) || Number <- Numbers]
    end,
    DisplaysRaw
  ),
  ValidDigitsCount = lists:filter(
    fun(Value) ->
      Value =/= -1
    end,
    lists:flatten(OutputValues)
  ),
  io:fwrite("Answer: ~p~n", [length(ValidDigitsCount)]),
  ok.

day8_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  DisplaysRaw = string:lexemes(Content, "\n"),
  DisplayLines = lists:map(
    fun(DisplayRaw) ->
      [Ciphers, Output] = string:split(DisplayRaw, " | ", all),
      NumbersRaw = [lists:sort(binary_to_list(NumberRaw)) || NumberRaw <- string:split(Output, " ", all)],
      CipherRaw = [lists:sort(binary_to_list(CipherLetter)) || CipherLetter <- string:split(Ciphers, " ", all)],
      CipherDict = generate_cipher_dictionary(CipherRaw),
      {CipherDict, NumbersRaw}
    end,
    DisplaysRaw
  ),
  OutputValues = lists:map(
    fun({CipherDict, Numbers}) ->
      really_decode_number(CipherDict, Numbers)
    end,
    DisplayLines
  ),
  io:fwrite("Answer: ~p~n", [lists:sum(OutputValues)]),
  ok.

decode_easy_number(Number) ->
  case length(Number) of
    2 -> 1;
    3 -> 7;
    4 -> 4;
    7 -> 8;
    _ -> -1
  end.

really_decode_number(CipherDict, Output) ->
  Digits = lists:map(
    fun(OutputNumber) -> 
      {Digit, _} = lists:keyfind(OutputNumber, 2, CipherDict),
      integer_to_list(Digit)
    end,
    Output),
  list_to_integer(lists:concat(Digits)).

generate_cipher_dictionary(Cipher) ->
  EmptyDict = lists:zip(lists:seq(0, 9), lists:duplicate(10, null)),
  SortedCipher = lists:sort(fun(A, B) -> length(A) =< length(B) end, Cipher),
  generate_partial_dictionary(SortedCipher, EmptyDict).

generate_partial_dictionary([Number1, _, _, _, _, _, _, _, _, _] = SortedCipher,
[{0, null}, {1, null}, {2, null}, {3, null}, {4, null}, {5, null}, {6, null}, {7, null}, {8, null}, {9, null}] = CipherDict) ->
  NewDict = lists:keyreplace(1, 1, CipherDict, {1, Number1}),
  generate_partial_dictionary(SortedCipher, NewDict);
generate_partial_dictionary([_, _, Number4, _, _, _, _, _, _, _] = SortedCipher, 
[{0, null}, {1, _}, {2, null}, {3, null}, {4, null}, {5, null}, {6, null}, {7, null}, {8, null}, {9, null}] = CipherDict) ->
  NewDict = lists:keyreplace(4, 1, CipherDict, {4, Number4}),
  generate_partial_dictionary(SortedCipher, NewDict);
generate_partial_dictionary([_, Number7, _, _, _, _, _, _, _, _] = SortedCipher,
  [{0, null}, {1, _}, {2, null}, {3, null}, {4, _}, {5, null}, {6, null}, {7, null}, {8, null}, {9, null}] = CipherDict) ->
  NewDict = lists:keyreplace(7, 1, CipherDict, {7, Number7}),
  generate_partial_dictionary(SortedCipher, NewDict);
generate_partial_dictionary([_, _, _, _, _, _, _, _, _, Number8] = SortedCipher,
  [{0, null}, {1, _}, {2, null}, {3, null}, {4, _}, {5, null}, {6, null}, {7, _}, {8, null}, {9, null}] = CipherDict) ->
  NewDict = lists:keyreplace(8, 1, CipherDict, {8, Number8}),
  generate_partial_dictionary(SortedCipher, NewDict);
generate_partial_dictionary([_, _, _, _, _, _, NumberUnknown1, NumberUnknown2, NumberUnknown3, _] = SortedCipher,
  [{0, null}, {1, Number1}, {2, null}, {3, null}, {4, Number4}, {5, null}, {6, null}, {7, _}, {8, _}, {9, null}] = CipherDict) ->
  Maybe6s = [NumberUnknown1, NumberUnknown2, NumberUnknown3],
  {[N6], ZeroOrNine} = lists:partition(fun(N) -> not number_contains(N, Number1) end, Maybe6s),
  {[N0], [N9]} = lists:partition(fun(N) -> not number_contains(N, Number4) end, ZeroOrNine),
  NextDict = merge(CipherDict, [{0, N0}, {6, N6}, {9, N9}]),
  generate_partial_dictionary(SortedCipher, NextDict);
generate_partial_dictionary([_, _, _, NumberUnknown1, NumberUnknown2, NumberUnknown3, _, _, _, _] = _SortedCipher,
  [{0, _}, {1, Number1}, {2, null}, {3, null}, {4, _}, {5, null}, {6, Number6}, {7, _}, {8, _}, {9, _}] = CipherDict) ->
  Maybe3s = [NumberUnknown1, NumberUnknown2, NumberUnknown3],
  {[N3], TwoOrFive} = lists:partition(fun(N) -> number_contains(N, Number1) end, Maybe3s),
  {[N2], [N5]} = lists:partition(fun(N) -> not number_contains(Number6, N) end, TwoOrFive),
  merge(CipherDict, [{2, N2}, {3, N3}, {5, N5}]).

number_contains(Number, SubNumber) ->
  lists:all(
    fun(Letter) ->
      lists:member(Letter, Number)
    end,
    SubNumber
  ).

merge(OriginalDict, OverwritingDict) ->
  ReplaceFun = fun({Key, Value}) -> {Key, proplists:get_value(Key, OverwritingDict, Value)} end,
  lists:map(ReplaceFun, OriginalDict).

day8_test() ->
  ?assertMatch(-1, decode_easy_number("bcefa")),
  ?assertMatch(1, decode_easy_number("ge")),
  ?assertMatch(7, decode_easy_number("efg")),
  ?assertMatch(4, decode_easy_number("efgb")),
  ?assertMatch(8, decode_easy_number("cefdgab")),

  CipherRaw = ["acedgfb", "bcdef", "acdfg", "abcdf", "abd", "abcdef", "bcdefg", "abef", "abcdeg", "ab"],
  CipherDict = [{0, "abcdeg"}, {1, "ab"}, {2, "acdfg"}, {3, "abcdf"}, {4, "abef"}, {5, "bcdef"}, {6, "bcdefg"}, {7, "abd"}, {8, "acedgfb"}, {9, "abcdef"}],
  ?assertMatch(CipherDict, generate_cipher_dictionary(CipherRaw)),

  Numbers = ["bcdef", "abcdf", "bcdef", "abcdf"],
  ?assertMatch(5353, really_decode_number(CipherDict, Numbers)),
  ok.

%%###############%%
%%     DAY 9     %%
%%###############%%

day9_part1() ->
  {ok, Content} = file:read_file("input.txt"),
  RowsRaw = [binary_to_list(RowRaw) || RowRaw <- string:lexemes(Content, "\n")],
  HeightsGrid = lists:map(
    fun(RowRaw) ->
      [list_to_integer([HeightAscii]) || HeightAscii <- RowRaw]
    end,
    RowsRaw),
  io:fwrite("Answer: ~p~n", [risk_level(HeightsGrid)]),
  ok.

day9_part2() ->
  {ok, Content} = file:read_file("input.txt"),
  RowsRaw = [binary_to_list(RowRaw) || RowRaw <- string:lexemes(Content, "\n")],
  HeightsGrid = lists:map(
    fun(RowRaw) ->
      [list_to_integer([HeightAscii]) || HeightAscii <- RowRaw]
    end,
    RowsRaw),
  io:fwrite("Answer: ~p~n", [get_product_of_3_biggest_basin_sizes(HeightsGrid)]),
  ok.

risk_level(HeightsGrid) ->
  GridHeight = get_grid_height(HeightsGrid),
  GridWidth = get_grid_width(HeightsGrid),
  AllPositions = [{X, Y} || X <- lists:seq(1, GridWidth), Y <- lists:seq(1, GridHeight)],
  risk_level(HeightsGrid, AllPositions, 0).

risk_level(HeightsGrid, [{X, Y} | NextPositions], RiskLevel) ->
  CurrentHeight = get_height(HeightsGrid, X, Y),
  NeighboursMaybePositions = [
    {X + 1, Y},
    {X - 1, Y},
    {X, Y + 1},
    {X, Y - 1}
  ],
  NeighboursPositions = lists:filter(
    fun({NeighbourX, NeighbourY}) ->
      GridHeight = get_grid_height(HeightsGrid),
      GridWidth = get_grid_width(HeightsGrid),
      NeighbourX > 0 andalso NeighbourY > 0 andalso NeighbourX =< GridWidth andalso NeighbourY =< GridHeight
    end,
    NeighboursMaybePositions
  ),
  NeighbourHeights = lists:map(
    fun({NeighbourX, NeighbourY}) ->
      get_height(HeightsGrid, NeighbourX, NeighbourY)
    end,
    NeighboursPositions
  ),
  IsRisky = lists:all(fun(NeighbourHeight) ->
    NeighbourHeight > CurrentHeight end, NeighbourHeights),
  CurrentRisk = if IsRisky -> (CurrentHeight + 1); true -> 0 end,
  CurrentRisk + risk_level(HeightsGrid, NextPositions, RiskLevel);
risk_level(_HeightsGrid, [], RiskLevel) ->
  RiskLevel.

get_height(HeightsGrid, X, Y) ->
  Row = lists:nth(Y, HeightsGrid),
  lists:nth(X, Row).

get_grid_height(HeightsGrid) ->
  length(HeightsGrid).

get_grid_width(HeightsGrid) ->
  length(lists:nth(1, HeightsGrid)).

get_product_of_3_biggest_basin_sizes(HeightsGrid) ->
  Basins = get_basins(HeightsGrid),
  DescendingBasins = lists:reverse(lists:sort(Basins)),
  lists:nth(1, DescendingBasins) * lists:nth(2, DescendingBasins) * lists:nth(3, DescendingBasins).

get_basins(HeightsGrid) ->
  GridHeight = get_grid_height(HeightsGrid),
  GridWidth = get_grid_width(HeightsGrid),
  AllPositions = [{X, Y} || X <- lists:seq(1, GridWidth), Y <- lists:seq(1, GridHeight)],
  Result = lists:search(
    fun({X, Y}) ->
      get_height(HeightsGrid, X, Y) /= 9
    end,
    AllPositions
  ),
  case Result of
    false ->
      [];
    {value, Seed} ->
      BasinPositions = get_basin_positions(HeightsGrid, [Seed], []),
      ModifiedGrid = lists:foldl(
        fun({X, Y}, Grid) ->
          OldRow = lists:nth(Y, Grid),
          NewRow = replacenth(X, 9, OldRow),
          NewGrid = replacenth(Y, NewRow, Grid),
          NewGrid
        end,
        HeightsGrid,
        BasinPositions
      ),
      [length(BasinPositions)] ++ get_basins(ModifiedGrid)
  end.

get_basin_positions(_HeightsGrid, [] = _Seeds, BasinPositions) ->
  BasinPositions;
get_basin_positions(HeightsGrid, [{HeadX, HeadY} = HeadSeed | TailSeeds] = _Seeds, BasinPositions) ->
  NeighboursMaybePositions = [
    {HeadX + 1, HeadY},
    {HeadX - 1, HeadY},
    {HeadX, HeadY + 1},
    {HeadX, HeadY - 1}
  ],
  NewSeeds = lists:filter(
    fun({X, Y}) ->
      GridHeight = get_grid_height(HeightsGrid),
      GridWidth = get_grid_width(HeightsGrid),
      IsInsideGrid = X > 0 andalso Y > 0 andalso X =< GridWidth andalso Y =< GridHeight,
      IsKnown = lists:member({X, Y}, TailSeeds) orelse lists:member({X, Y}, BasinPositions),
      IsInsideGrid andalso not IsKnown andalso get_height(HeightsGrid, X, Y) /= 9
    end,
    NeighboursMaybePositions
  ),
  NewBasinPositions = [HeadSeed] ++ BasinPositions,
  get_basin_positions(HeightsGrid, NewSeeds ++ TailSeeds, NewBasinPositions).

replacenth(Index, Value, List) ->
  replacenth(Index - 1, Value, List, [], 0).

replacenth(ReplaceIndex, Value, [_ | List], Acc, ReplaceIndex) ->
  lists:reverse(Acc) ++ [Value | List];
replacenth(ReplaceIndex, Value, [V | List], Acc, Index) ->
  replacenth(ReplaceIndex, Value, List, [V | Acc], Index + 1).

day9_test() ->
  HeightsGrid = [
    [9, 9, 9, 9],
    [9, 1, 9, 1],
    [1, 9, 9, 9]
  ],
  ?assertMatch(6, risk_level(HeightsGrid)),

  HeightsGridBasins = [
    [3, 3, 9, 3, 3],
    [3, 9, 9, 3, 9],
    [9, 4, 4, 9, 2],
    [9, 4, 4, 9, 2]
  ],

  ?assertMatch(36, get_product_of_3_biggest_basin_sizes(HeightsGridBasins)),

  ok.
