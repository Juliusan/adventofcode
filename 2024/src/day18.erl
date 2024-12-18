-module(day18).
-export([solve_1/3, solve_2/3]).

% Pagaliau išmokau vaiškčioti per žemėlapį. Užtruko tik užrašymas. Iš esmės,
% kopijavau nuo 16 dienos ir išmečiau tai, ko nereikia. Antrą dalį dariau
% primityviai. Gavosi greitai, bet nelabai efektyviai: apie 5 sekundes. Turiu
% minčių, ką reikia patobulinti. Prisėdau. Patobulinau. Keli šimtai
% milisekundžių dabar. Pakako prisiminti gerą kelią ir jį perskaičiuoti tik
% jei ant jo esantį langelį sugadina.

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
% {161263,"26,22"}


read_inputs(FileName) ->
    utils:read_lines_no_new_line_to_elems(FileName, fun(Line) ->
        [X, Y] = utils:get_integer_list(Line, ","),
        {X, Y}
    end).


index_to_map({Row, Col}) -> {Row+1, Col+1}.

place_corrupted(Index, Map) ->
    Map#{index_to_map(Index) => $#}.

place_init_corrupted(Indexes, Dimensions) ->
    MapEmpty = utils:get_new_matrix($., Dimensions),
    lists:foldl(fun(Index, Acc) ->
        place_corrupted(Index, Acc)
    end, MapEmpty, Indexes).


walk_map(Map, Dimensions) ->
    walk_map({1,1}, Map,  Dimensions).

walk_map(StartIndex, Map, Dimensions) ->
    walk_map([StartIndex], Map, Dimensions, #{StartIndex => {0, []}}).

walk_map([],              _,   _,          Distances) -> Distances;
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
                        {PrevDistance, PrevPath} = maps:get(Index, AccDistances),
                        NewDistance = PrevDistance + 1,
                        NewPath = [Index | PrevPath],
                        {OldDistance, _} = maps:get(NewIndex, AccDistances, {infinity, undefined}),
                        case NewDistance < OldDistance of
                            true  ->
                                NewAccTiles = AccTiles ++ [NewIndex],
                                NewAccDistances = AccDistances#{NewIndex => {NewDistance, NewPath}},
                                {NewAccTiles, NewAccDistances};
                            false ->
                                {AccTiles, AccDistances}
                        end
                end
        end
    end, {Tiles, Distances}, Directions),
    walk_map(NewTiles, Map, Dimensions, NewDistances).


solve(FileName, Dimensions, Bytes) ->
    Corrupted = lists:reverse(read_inputs(FileName)),
    RealCorrupted = lists:sublist(Corrupted, Bytes),
    Map = place_init_corrupted(RealCorrupted, Dimensions),
    RemCorrupted = lists:nthtail(Bytes, Corrupted),
    Distances = walk_map(Map, Dimensions),
    {Distance, Path} = maps:get(Dimensions, Distances),
    {RemCorrupted, Map, Distance, Path}.



solve_1(FileName, Dimensions, Bytes) ->
    {_, _, Distance, _} = solve(FileName, Dimensions, Bytes),
    Distance.


solve_2(FileName, Dimensions, Bytes) ->
    {RemCorrupted, Map, _, InitPath} = solve(FileName, Dimensions, Bytes),
    Fun = fun Fun([{Row,Col}=Index | Indexes], AccMap, PathToEnd) ->
        NewAccMap = place_corrupted(Index, AccMap),
        case lists:member(index_to_map(Index), PathToEnd) of
            true ->
                NewDistances = walk_map(NewAccMap, Dimensions),
                case maps:get(Dimensions, NewDistances, undefined) of
                    undefined -> lists:append(lists:join(",",[erlang:integer_to_list(Row), erlang:integer_to_list(Col)]));
                    {_, Path} -> Fun(Indexes, NewAccMap, Path)
                end;
            false ->
                Fun(Indexes, NewAccMap, PathToEnd)
        end
    end,
    Fun(RemCorrupted, Map, InitPath).
