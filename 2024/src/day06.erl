-module(day06).
-export([solve_1/1, solve_2/1]).

% TODO

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day06:solve_1("priv/day06-PVZ.txt").
% 143
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day05:solve_1("priv/day05.txt").
% 5955
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day05:solve_2("priv/day05-PVZ.txt").
% 123
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day05:solve_2("priv/day05.txt").
% 4030


read_inputs(FileName) ->
    MapLines = lists:reverse(utils:read_lines_no_new_line(FileName)),
    [FirstLine|_] = MapLines,
    Cols = erlang:length(FirstLine),
    {Rows1, Map} = lists:foldl(fun(Line, {Row, Acc}) ->
        {Cols1, NewAcc} = lists:foldl(fun(Pos, {Col, Accc}) ->
            NewAccc = Accc#{{Row, Col} => Pos},
            {Col+1, NewAccc}
        end, {1, Acc}, Line),
        Cols = Cols1-1,
        {Row+1,NewAcc}
    end, {1, #{}}, MapLines),
    {Map, Rows1-1, Cols}.


find_in_map(Elem, Map) ->
    FindValueFun = fun FindValueFun(It) ->
        case maps:next(It) of
            {Location, Elem, _     } -> Location;
            {_,        _,    NextIt} -> FindValueFun(NextIt);
            none                     -> undefined
        end
    end,
    FindValueFun(maps:iterator(Map)).


next_index(_Rows, _Cols,  1,    _Col,  up   ) -> undefined;
next_index(_Rows, _Cols,  Row,   Col,  up   ) -> {Row-1, Col};
next_index(_Rows,  Cols, _Row,   Cols, right) -> undefined;
next_index(_Rows, _Cols,  Row,   Col,  right) -> {Row, Col+1};
next_index( Rows, _Cols,  Rows, _Col,  down ) -> undefined;
next_index(_Rows, _Cols,  Row,   Col,  down ) -> {Row+1, Col};
next_index(_Rows, _Cols, _Row,   1,    left ) -> undefined;
next_index(_Rows, _Cols,  Row,   Col,  left ) -> {Row, Col-1}.


turn(up   ) -> right;
turn(right) -> down;
turn(down ) -> left;
turn(left ) -> up.


print_map(Map, Rows, Cols) ->
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Col) ->
            io:fwrite("~s", [[maps:get({Row, Col}, Map)]])
        end, lists:seq(1, Cols)),
        io:fwrite("~n")
    end, lists:seq(1, Rows)).


walk_map(Map, Rows, Cols, AccStepCount, [{Row, Col, Direction}|_] = AccPath) ->
    Next = next_index(Rows, Cols, Row, Col, Direction),
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
    %print_map(Map, Rows, Cols),
    {Row, Col} = find_in_map($^, Map),
    MapNoS = Map#{{Row, Col} => $X},
    %utils:print("Start: ~p ~p", [Row, Col]),
    {StepCount, _} = walk_map(MapNoS, Rows, Cols, 1, [{Row, Col, up}]),
    StepCount.


solve_2(FileName) ->
    {Map, Rows, Cols} = read_inputs(FileName),
    {RowS, ColS} = find_in_map($^, Map),
    MapNoS = Map#{{RowS, ColS} => $X},
    {_, MapWalked} = walk_map(MapNoS, Rows, Cols, 1, [{RowS, ColS, up}]),
    print_map(MapWalked, Rows, Cols),
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            %utils:print("XXX ~p ~p", [Row, Col]),
            case maps:get({Row, Col}, MapWalked) of
                $^ -> Accc;
                $# -> Accc;
                $. -> Accc;
                $X ->
                    MapNewOb = MapNoS#{{Row, Col} => $#},
                    case walk_map(MapNewOb, Rows, Cols, 0, [{RowS, ColS, up}]) of
                        loop -> utils:print("XXX ~p ~p", [Row, Col]), Accc+1;
                        _    -> Accc
                    end
            end
        end, Acc, lists:seq(1, Cols))
    end, 0, lists:seq(1, Rows)).
