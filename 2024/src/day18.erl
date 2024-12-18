-module(day18).
-export([solve_1/3, solve_2/3]).

% Pagaliau išmokau vaiškčioti per žemėlapį. Užtruko tik užrašymas. Iš esmės,
% kopijavau nuo 16 dienos ir išmečiau tai, ko nereikia. Antrą dalį dariau
% primityviai. Gavosi greitai, bet nelabai efektyviai: apie 5 sekundes. Turiu
% minčių, ką reikia patobulinti.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day18:solve_1("priv/day18-PVZ.txt", {7,7}, 12).
% 22
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day18:solve_1("priv/day18.txt", {71,71}, 1024).
% 438
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day18:solve_2("priv/day18-PVZ.txt", {7,7}, 12).
% "6,1"
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day18:solve_2("priv/day18.txt", {71,71}, 1024).
% "26,22"
% (aoc_2024@JuliusErisataT14.erisata.lt)5> timer:tc(fun() -> day18:solve_1("priv/day18.txt", {71,71}, 1024) end).
% {42086,438}
% (aoc_2024@JuliusErisataT14.erisata.lt)6> timer:tc(fun() -> day18:solve_2("priv/day18.txt", {71,71}, 1024) end).
% {4852024,"26,22"}


read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        [X, Y] = utils:get_integer_list(Line, ","),
        {X, Y}
    end).


place_corrupted(Indexes, Dimensions) ->
    MapEmpty = utils:get_new_matrix($., Dimensions),
    lists:foldl(fun({Row, Col}, Acc) ->
        Acc#{{Row+1,Col+1} => $#}
    end, MapEmpty, Indexes).


walk_map([],                                  _,   _,          Distances) -> Distances;
walk_map([Index | Tiles], Map, Dimensions, Distances) ->
    Directions = utils:direction_all(),
    {NewTiles, NewDistances} = lists:foldl(fun(NewDirection, {AccTiles, AccDistances}) ->
        NewIndex = utils:matrix_next_index(Index, NewDirection, Dimensions),
        case NewIndex of
            undefined -> {AccTiles, AccDistances};
            _ ->
                case maps:get(NewIndex, Map) of
                    $# -> {AccTiles, AccDistances};
                    $. ->
                        PrevDistance = maps:get(Index, Distances),
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


solve_1(FileName, Dimensions, Bytes) ->
    Corrupted = read_inputs(FileName),
    RealCorrupted = lists:sublist(lists:reverse(Corrupted), Bytes),
    Map = place_corrupted(RealCorrupted, Dimensions),
    Distances = walk_map([{1,1}], Map, Dimensions, #{{1,1} => 0}),
    maps:get(Dimensions, Distances).


solve_2(FileName, Dimensions, Bytes) ->
    Corrupted = lists:reverse(read_inputs(FileName)),
    RealCorrupted = lists:sublist(Corrupted, Bytes),
    Map = place_corrupted(RealCorrupted, Dimensions),
    Fun = fun Fun([{Row,Col}|Indexes], AccMap) ->
        NewAccMap = AccMap#{{Row+1,Col+1} => $#},
        Distances = walk_map([{1,1}], NewAccMap, Dimensions, #{{1,1} => 0}),
        case maps:get(Dimensions, Distances, undefined) of
            undefined -> lists:append(lists:join(",",[erlang:integer_to_list(Row), erlang:integer_to_list(Col)]));
            _         -> Fun(Indexes, NewAccMap)
        end
    end,
    Fun(lists:nthtail(Bytes, Corrupted), Map).
