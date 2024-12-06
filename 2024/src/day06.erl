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


walk_map(Map, Rows, Cols, AccStepCount, [{Row, Col, Direction}|_] = AccPath) ->
    Next = utils:matrix_next_index(Row, Col, Direction, Rows, Cols),
    case Next of
        undefined ->
            {AccStepCount, Map#{{Row, Col} => $X}};
        {RowN, ColN} ->
            {Step, NewAccStepCount} = case maps:get({RowN, ColN}, Map) of
                $. -> {go,   AccStepCount+1};
                $X -> {go,   AccStepCount};
                $# -> {turn, AccStepCount}
            end,
            NextPathElem = case Step of
                go   ->                                 {RowN, ColN, Direction};
                turn -> NewDirection = turn(Direction), {Row,  Col,  NewDirection}
            end,
            case lists:member(NextPathElem, AccPath) of
                true  -> loop;
                false -> walk_map(Map#{{Row, Col} => $X}, Rows, Cols, NewAccStepCount, [NextPathElem|AccPath])
            end
    end.



solve_1(FileName) ->
    {Map, Rows, Cols} = read_inputs(FileName),
    {Row, Col} = utils:matrix_index_of($^, Map),
    MapNoS = Map#{{Row, Col} => $X},
    {StepCount, _} = walk_map(MapNoS, Rows, Cols, 1, [{Row, Col, up}]),
    StepCount.


solve_2(FileName) ->
    {Map, Rows, Cols} = read_inputs(FileName),
    {RowS, ColS} = utils:matrix_index_of($^, Map),
    MapNoS = Map#{{RowS, ColS} => $X},
    {_, MapWalked} = walk_map(MapNoS, Rows, Cols, 1, [{RowS, ColS, up}]),
    utils:matrix_foldl(fun
        (_,   _,   $#, Acc) -> Acc;
        (_,   _,   $., Acc) -> Acc;
        (Row, Col, $X, Acc) ->
            MapNewOb = MapNoS#{{Row, Col} => $#},
            case walk_map(MapNewOb, Rows, Cols, 1, [{RowS, ColS, up}]) of
                loop -> Acc+1;
                _    -> Acc
            end
    end, 0, MapWalked, Rows, Cols).
