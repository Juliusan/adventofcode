-module(day07).
-export([solve_1/1, solve_2/1]).


%

% (aoc_2025@JuliusErisataT14.erisata.lt)1> day07:solve_1("priv/day07-PVZ.txt").


solve_1(FileName) ->
    Lines = ja_erl_utils_file:read_lines_no_new_line(FileName),
    Map = ja_erl_utils_matrix:get_char_matrix(lists:reverse(Lines)),
    Start = ja_erl_utils_matrix:index_of($S, Map),
    %ja_erl_utils_terminal:print("~p", [Start]),
    {Rows, _Columns} = ja_erl_utils_matrix:dimensions(Map),
    CheckSplitFun = fun
        CheckSplitFun(_Beams, Splits, Row) when Row > Rows ->
            Splits;
        CheckSplitFun(Beams, Splits, Row) ->
            {NewSplits, NewBeams} = lists:foldl(fun({_, Column}, {AccSplits, Indices}) ->
                Index = {Row, Column},
                case ja_erl_utils_matrix:get(Index, Map) of
                    $. ->
                        {AccSplits, [Index | Indices]};
                    $^ ->
                        NewAccIndices = lists:foldl(fun(Direction, AccIndices) ->
                            case ja_erl_utils_matrix:next_index(Index, Direction, Map) of
                                undefined -> AccIndices;
                                NextIndex -> [NextIndex|AccIndices]
                            end
                        end, Indices, [left, right]),
                        {AccSplits + 1, NewAccIndices}
                end
            end, {Splits, []}, Beams),
            NewBeamsSorted = lists:usort(NewBeams),
            %ja_erl_utils_terminal:print("~p", [NewBeamsSorted]),
            CheckSplitFun(NewBeamsSorted, NewSplits, Row+1)
    end,
    CheckSplitFun([Start], 0, 2).


solve_2(FileName) ->
    Lines = ja_erl_utils_file:read_lines_no_new_line(FileName),
    Map = ja_erl_utils_matrix:get_char_matrix(lists:reverse(Lines)),
    Start = ja_erl_utils_matrix:index_of($S, Map),
    %ja_erl_utils_terminal:print("~p", [Start]),
    {Rows, _Columns} = ja_erl_utils_matrix:dimensions(Map),
    CheckSplitFun = fun
        CheckSplitFun(Beams, _Splits, Row) when Row > Rows ->
            ja_erl_utils_list:map_sum(fun({_, Path}) -> Path end, Beams);
        CheckSplitFun(Beams, Splits, Row) ->
            {NewSplits, NewBeams} = lists:foldl(fun({{_, Column}, Paths}, {AccSplits, Indices}) ->
                Index = {Row, Column},
                case ja_erl_utils_matrix:get(Index, Map) of
                    $. ->
                        {AccSplits, [{Index, Paths} | Indices]};
                    $^ ->
                        NewAccIndices = lists:foldl(fun(Direction, AccIndices) ->
                            case ja_erl_utils_matrix:next_index(Index, Direction, Map) of
                                undefined -> AccIndices;
                                NextIndex -> [{NextIndex, Paths}|AccIndices]
                            end
                        end, Indices, [left, right]),
                        {AccSplits + 1, NewAccIndices}
                end
            end, {Splits, []}, Beams),
            JoinIndicesFun = fun
                JoinIndicesFun([], AccIndices) ->
                    AccIndices;
                JoinIndicesFun([{Index,Path1}, {Index, Path2} | RemIndices], AccIndices) ->
                    JoinIndicesFun(RemIndices, [{Index, Path1+Path2}|AccIndices]);
                JoinIndicesFun([IndexPath|RemIndices], AccIndices) ->
                    JoinIndicesFun(RemIndices, [IndexPath|AccIndices])
            end,
            NewBeamsJoined = JoinIndicesFun(NewBeams, []),
            %ja_erl_utils_terminal:print("~p", [NewBeamsJoined]),
            CheckSplitFun(NewBeamsJoined, NewSplits, Row+1)
    end,
    CheckSplitFun([{Start, 1}], 0, 2).
