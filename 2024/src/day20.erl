-module(day20).
-export([solve_1/2, solve_2/2]).

% Šiandien labai neturėjau laiko spręsti. Sprendžiau priepuoliais, ir tai
% matosi. Algoritmą pirmai daliai sugalvojau greitai, bet pusę laiko praleidau
% ne jį įgyvendindamas, o gaudydamas vaikišką bugą: sumaišiau vidinio ir
% išorinio foldo akumuliatorius. Tai gaila, kad tiek laiko praleidi ieškodamas
% tokių kvailų klaidų. Antrą dalį irgi greitai sugalvojau, kaip padaryti, o ir
% padaryti ilgai netruko. Tiesa, antros dalies laikas - 3s. Daugoka. Na bet
% neturiu idėjo, o ir laiko ką nors tobulinti. Nepaisant to, po truputį darausi
% vaikščiojimų po žemėlapį ekspertas.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day20:solve_1("priv/day20-PVZ.txt", 0).
% 44
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day20:solve_1("priv/day20.txt", 100).
% 1530
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day20:solve_2("priv/day20-PVZ.txt", 50).
% 285
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day20:solve_2("priv/day20.txt", 100).
% 1033983
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day20:solve_1("priv/day20.txt", 100) end).
% {107287,1530}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day20:solve_2("priv/day20.txt", 100) end).
% {2865640,1033983}


read_inputs(FileName) ->
    Lines = utils:read_lines_no_new_line(FileName),
    utils:get_char_matrix(Lines).


walk_map(StartIndex, Map, Dimensions) ->
    walk_map([StartIndex], Map, Dimensions, #{StartIndex => 0}).

walk_map([],              _,   _,          Distances) -> Distances;
walk_map([Index | Tiles], Map, Dimensions, Distances) ->
    Directions = utils:direction_all(),
    {NewTiles, NewDistances} = lists:foldl(fun(NewDirection, {AccTiles, AccDistances}) ->
        NewIndex = utils:matrix_next_index(Index, NewDirection, Dimensions),
        case NewIndex of
            undefined ->
                {AccTiles, AccDistances};
            _ ->
                case maps:get(NewIndex, Map) of
                    $# ->
                        {AccTiles, AccDistances};
                    C when C =:= $.; C =:= $S; C =:= $E ->
                        PrevDistance = maps:get(Index, AccDistances),
                        NewDistance = PrevDistance + 1,
                        OldDistance = maps:get(NewIndex, AccDistances, infinity),
                        case NewDistance < OldDistance of
                            true  ->
                                NewAccTiles = AccTiles ++ [NewIndex],
                                NewAccDistances = AccDistances#{NewIndex => NewDistance},
                                {NewAccTiles, NewAccDistances};
                            false ->
                                {AccTiles, AccDistances}
                        end
                end
        end
    end, {Tiles, Distances}, Directions),
    walk_map(NewTiles, Map, Dimensions, NewDistances).


find_good_cheats(FromStart, FromEnd, MaxCheatLength, Map, Dimensions, PathLength) ->
    maps:fold(fun
        (_Index, $#, Acc) -> Acc;
        (IndexS,  CS, Acc) when CS =:= $.; CS =:= $S; CS =:= $E ->
            lists:foldl(fun(CheatLength, Acc1) ->
                lists:foldl(fun(RowAdj, Acc2) ->
                    ColAdj = CheatLength - RowAdj,
                    {Row, Col} = IndexS,
                    IndexEs = [{Row+RowAdj, Col+ColAdj}, {Row-RowAdj, Col+ColAdj}, {Row+RowAdj, Col-ColAdj}, {Row-RowAdj, Col-ColAdj}],
                    ExistingIndexEs = lists:usort(lists:filter(fun(I) -> utils:matrix_is_valid_index(I, Dimensions) end, IndexEs)),
                    lists:foldl(fun(IndexE, Acc3) ->
                        case maps:get(IndexE, Map) of
                            $# -> Acc3;
                            CE when CE =:= $.; CE =:= $S; CE =:= $E ->
                                LengthFromStart = maps:get(IndexS, FromStart, undefined),
                                LengthFromEnd   = maps:get(IndexE, FromEnd, undefined),
                                case {LengthFromStart, LengthFromEnd} of
                                    {undefined, _} -> Acc3;
                                    {_, undefined} -> Acc3;
                                    {_, _        } ->
                                        NewLength = LengthFromStart + CheatLength + LengthFromEnd,
                                        case NewLength < PathLength of
                                            true  -> [{IndexS, IndexE, NewLength} | Acc3];
                                            false -> Acc3
                                        end
                                end
                        end
                    end, Acc2, ExistingIndexEs)
                end, Acc1, lists:seq(0, CheatLength))
            end, Acc, lists:seq(2, MaxCheatLength))
    end, [], Map).


solve(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    Start = utils:matrix_index_of($S, Map),
    DistancesFromStart = walk_map(Start, Map, Dimensions),
    End = utils:matrix_index_of($E, Map),
    Length = maps:get(End, DistancesFromStart),
    DistancesFromEnd = walk_map(End, Map, Dimensions),
    Length = maps:get(Start, DistancesFromEnd),
    {Map, Dimensions, DistancesFromStart, DistancesFromEnd, Length}.


solve_1(FileName, AtLeast) ->
    {Map, Dimensions, DistancesFromStart, DistancesFromEnd, Length} = solve(FileName),
    GoodCheats = find_good_cheats(DistancesFromStart, DistancesFromEnd, 2, Map, Dimensions, Length),
    utils:list_filter_count(fun({_, _, NewLength}) ->
        Length - NewLength >= AtLeast
    end, GoodCheats).


solve_2(FileName, AtLeast) ->
    {Map, Dimensions, DistancesFromStart, DistancesFromEnd, Length} = solve(FileName),
    GoodCheats = find_good_cheats(DistancesFromStart, DistancesFromEnd, 20, Map, Dimensions, Length),
    utils:list_filter_count(fun({_, _, NewLength}) ->
        Length - NewLength >= AtLeast
    end, GoodCheats).
