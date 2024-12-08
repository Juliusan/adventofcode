-module(day06).
-export([solve_1/1, solve_2/1]).

% Užduotis, ypač pirmoji, nesudėtinga. Privertė pasidaryti darbo su dvimačiais
% masyvais pagalbines funkcijas. Antroje dalyje dariau pačiu primityviausiu
% perrinkimo būdu, supratau, kad kiek per ilgai trunka, tada truputį pagalvojau
% ir dėžes dėjau tik į tas vietas, kuriomis sargas tikrai vaikšto. Tai sumažino
% paiešką 3-4 kartus, o to užteko, kad programa sutilptų į 3 su trupučiu minutės.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day06:solve_1("priv/day06-PVZ.txt").
% 41
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day06:solve_1("priv/day06.txt").
% 4982
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day06:solve_2("priv/day06-PVZ.txt").
% 6
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day06:solve_2("priv/day06.txt").
% 1663


read_inputs(FileName) ->
    MapLines = lists:reverse(utils:read_lines_no_new_line(FileName)),
    utils:get_char_matrix(MapLines).


turn(up   ) -> right;
turn(right) -> down;
turn(down ) -> left;
turn(left ) -> up.


walk_map(Map, Dimensions, AccStepCount, [{Index, Direction}|_] = AccPath) ->
    Next = utils:matrix_next_index(Index, Direction, Dimensions),
    case Next of
        undefined ->
            {AccStepCount, Map#{Index => $X}};
        IndexN ->
            {Step, NewAccStepCount} = case maps:get(IndexN, Map) of
                $. -> {go,   AccStepCount+1};
                $X -> {go,   AccStepCount};
                $# -> {turn, AccStepCount}
            end,
            NextPathElem = case Step of
                go   ->                                 {IndexN, Direction};
                turn -> NewDirection = turn(Direction), {Index,  NewDirection}
            end,
            case lists:member(NextPathElem, AccPath) of
                true  -> loop;
                false -> walk_map(Map#{Index => $X}, Dimensions, NewAccStepCount, [NextPathElem|AccPath])
            end
    end.



solve_1(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    IndexS = utils:matrix_index_of($^, Map),
    MapNoS = Map#{IndexS => $X},
    {StepCount, _} = walk_map(MapNoS, Dimensions, 1, [{IndexS, up}]),
    StepCount.


solve_2(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    IndexS = utils:matrix_index_of($^, Map),
    MapNoS = Map#{IndexS => $X},
    {_, MapWalked} = walk_map(MapNoS, Dimensions, 1, [{IndexS, up}]),
    utils:matrix_foldl(fun
        (_,     $#, Acc) -> Acc;
        (_,     $., Acc) -> Acc;
        (Index, $X, Acc) ->
            MapNewOb = MapNoS#{Index => $#},
            case walk_map(MapNewOb, Dimensions, 1, [{IndexS, up}]) of
                loop -> Acc+1;
                _    -> Acc
            end
    end, 0, MapWalked, Dimensions).
