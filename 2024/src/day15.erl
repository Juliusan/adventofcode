-module(day15).
-export([solve_1/1, solve_2/1]).

% Daug mąstymo ir programavimo. Nors dalys atrodo panašios, tai kad antroje
% dalyje eilutės dvigubos, o stulpeliai ne, įneša savų komplikacijų. Visai
% įdomu buvo pasprendėti. Ne ilgiausiai užtrukau. 9 dienos pirma dalis
% truko ilgiau.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day15:solve_1("priv/day15-PVZ1.txt").
% 2028
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day15:solve_1("priv/day15-PVZ2.txt").
% 10092
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day15:solve_1("priv/day15.txt").
% 1398947
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day15:solve_2("priv/day15-PVZ2.txt").
% 9021
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day15:solve_2("priv/day15.txt").
% 1397393
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day15:solve_1("priv/day15.txt") end).
% {37962,1398947}
% (aoc_2024@JuliusErisataT14.erisata.lt)7> timer:tc(fun() -> day15:solve_2("priv/day15.txt") end).
% {46996,1397393}


read_inputs(FileName) ->
    [MapLines, Moves] = utils:read_lines_no_new_line_to_elems(FileName, [
        fun(Line) -> Line end,
        fun(Line) ->
            lists:map(fun
                ($>) -> right;
                ($v) -> down;
                ($<) -> left;
                ($^) -> up
            end, Line)
        end
    ], "\n"),
    {Map, Dimensions} = utils:get_char_matrix(lists:reverse(MapLines)),
    {Map, Dimensions, lists:append(lists:reverse(Moves))}.


widen_map(Map) ->
    maps:fold(fun({Row, Column}, Value, Acc) ->
        {New1, New2} = case Value of
            $# -> {$#, $#};
            $O -> {$[, $]};
            $. -> {$., $.};
            $@ -> {$@, $.}
        end,
        Acc#{{Row, Column*2-1} => New1, {Row, Column*2} => New2}
    end, #{}, Map).


find_empty(Index, Direction, Map, Dimensions, Acc) ->
    NextIndex = utils:matrix_next_index(Index, Direction, Dimensions),
    case maps:get(NextIndex, Map) of
        $# -> undefined;
        $O -> find_empty(NextIndex, Direction, Map, Dimensions, Acc+1);
        $. -> {NextIndex, Acc}
    end.


move_1(_,     [],           Map, _         ) -> Map;
move_1(Robot, [Move|Moves], Map, Dimensions) ->
    {NewMap, NewRobot} = case find_empty(Robot, Move, Map, Dimensions, 0) of
        undefined ->
            {Map, Robot};
        {Empty, Boxes} ->
            NewMap1 = Map#{Robot => $.},
            Reverse = utils:direction_reverse(Move),
            {NewMap2, NRobot} = lists:foldl(fun(_, {AccMap, Index}) ->
                NewAccMap = AccMap#{Index => $O},
                NewIndex = utils:matrix_next_index(Index, Reverse, Dimensions),
                {NewAccMap, NewIndex}
            end, {NewMap1, Empty}, lists:seq(1,Boxes)),
            NewMap3 = NewMap2#{NRobot => $@},
            {NewMap3, NRobot}
    end,
    move_1(NewRobot, Moves, NewMap, Dimensions).


find_empty_hor(Index, Direction, Map, Dimensions, Acc) ->
    NextIndex = utils:matrix_next_index(Index, Direction, Dimensions),
    NextNextIndex = utils:matrix_next_index(NextIndex, Direction, Dimensions),
    case {maps:get(NextIndex, Map), maps:get(NextNextIndex, Map), Direction} of
        {$#, _,  _    } -> undefined;
        {$[, $], right} -> find_empty_hor(NextNextIndex, Direction, Map, Dimensions, Acc+1);
        {$], $[, left } -> find_empty_hor(NextNextIndex, Direction, Map, Dimensions, Acc+1);
        {$., _,  _    } -> {NextIndex, Acc}
    end.


find_empty_ver([], _, _, _, {AccMap, AccCleanIndexes}) -> {AccCleanIndexes, AccMap};
find_empty_ver([Index|Indexes], Direction, Map, Dimensions, {AccMap, AccCleanIndexes}) ->
    Value = maps:get(Index, Map),
    NextIndex = utils:matrix_next_index(Index, Direction, Dimensions),
    NewIndexes = case {Value, maps:get(NextIndex, Map)} of
        {_,  $#} -> undefined;
        {$[, $[} -> [NextIndex|Indexes];
        {$], $]} -> [NextIndex|Indexes];
        {$[, $]} -> [NextIndex,utils:matrix_next_index(NextIndex, left,  Dimensions)|Indexes];
        {$@, $]} -> [NextIndex,utils:matrix_next_index(NextIndex, left,  Dimensions)|Indexes];
        {$], $[} -> [NextIndex,utils:matrix_next_index(NextIndex, right, Dimensions)|Indexes];
        {$@, $[} -> [NextIndex,utils:matrix_next_index(NextIndex, right, Dimensions)|Indexes];
        {_,  $.} -> Indexes
    end,
    case NewIndexes of
        undefined -> undefined;
        _         -> find_empty_ver(NewIndexes, Direction, Map, Dimensions, {AccMap#{NextIndex => Value}, [Index|AccCleanIndexes]})
    end.


move_2(_, [], Map, _) -> Map;
move_2(Robot, [Move|Moves], Map, Dimensions) when Move =:= left; Move =:= right ->
    {NewMap, NewRobot} = case find_empty_hor(Robot, Move, Map, Dimensions, 0) of
        undefined ->
            {Map, Robot};
        {Empty, Boxes} ->
            NewMap1 = Map#{Robot => $.},
            Reverse = utils:direction_reverse(Move),
            {NewMap2, NRobot} = lists:foldl(fun(_, {AccMap, Index1}) ->
                Index2 = utils:matrix_next_index(Index1, Reverse, Dimensions),
                NewAccMap = case Reverse of
                    left  -> AccMap#{Index1 => $], Index2 => $[};
                    right -> AccMap#{Index1 => $[, Index2 => $]}
                end,
                NewIndex = utils:matrix_next_index(Index2, Reverse, Dimensions),
                {NewAccMap, NewIndex}
            end, {NewMap1, Empty}, lists:seq(1,Boxes)),
            NewMap3 = NewMap2#{NRobot => $@},
            {NewMap3, NRobot}
    end,
    move_2(NewRobot, Moves, NewMap, Dimensions);
move_2(Robot, [Move|Moves], Map, Dimensions) when Move =:= up; Move =:= down ->
    {NewMap, NewRobot} = case find_empty_ver([Robot], Move, Map, Dimensions, {#{}, []}) of
        undefined ->
            {Map, Robot};
        {CleanIndexes, SetMap} ->
            NewMap1 = lists:foldl(fun(Index, Acc) -> Acc#{Index => $.} end, Map, CleanIndexes),
            maps:fold(fun(Index, Value, {AccMap, AccRobot}) ->
                NewAccMap = AccMap#{Index => Value},
                NewAccRobot = case {Value, AccRobot} of
                    {$@, undefined} -> Index;
                    {$@, _        } -> error;
                    {_, _         } -> AccRobot
                end,
                {NewAccMap, NewAccRobot}
            end, {NewMap1, undefined}, SetMap)
    end,
    move_2(NewRobot, Moves, NewMap, Dimensions).




count_boxes(Map) ->
    utils:map_map_sum(fun
        ({Row, Col}, $O) -> 100*(Row-1) + Col-1;    % For first part
        ({Row, Col}, $[) -> 100*(Row-1) + Col-1;    % For second part
        (_Index,     _ ) -> 0
    end, Map).


solve_1(FileName) ->
    {Map, Dimensions, Moves} = read_inputs(FileName),
    Robot = utils:matrix_index_of($@, Map),
    NewMap = move_1(Robot, Moves, Map, Dimensions),
    count_boxes(NewMap).


solve_2(FileName) ->
    {Map, Dimensions, Moves} = read_inputs(FileName),
    {Rows, Cols} = Dimensions,
    WiderMap = widen_map(Map),
    Robot = utils:matrix_index_of($@, WiderMap),
    NewMap = move_2(Robot, Moves, WiderMap, {Rows, Cols*2}),
    count_boxes(NewMap).
