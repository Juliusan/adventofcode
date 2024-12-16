-module(day16).
-export([solve_1/1, solve_2/1]).

% Reikia man pagaliau išmokti vaikščioti po labirintą. Pirmą dalį turėjau
% gerokai pagalvoti, kaip daryti. Bet paskui sugalvojau ir padariau beveik iš
% karto gerai. Beje, pirmoji versija sukosi 9 sekundes, pagalvojus geresnę
% krypčių parinkimo tvarką - 7 sekundes. Ir tik kai pradėjau naujus langelius
% dėti ne į neperžiūrėtų langelių sąrašo pradžią, o į galą, tik tada laikas
% sumažėjo iki kelių šimtų milisekundžių. Koks didelis paieškos į gylį ir į
% plotį skirtumas! Antrai daliai buvau iš karto pasiruošęs. Tik pirmoje
% versijoje neatsižvelgdavau į pasisukimus ir gaudavau mažesnį atsakymą. Tik
% kai pradėjau ieškoti ne tik lengviausio kelio, bet lengviausio įskaitant
% pasisukimą, tik tada pradėjau gauti gerus atsakymus.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day16:solve_1("priv/day16-PVZ1.txt").
% 7036
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day16:solve_1("priv/day16-PVZ2.txt").
% 11048
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day16:solve_1("priv/day16.txt").
% 85480
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day16:solve_2("priv/day16-PVZ1.txt").
% 45
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day16:solve_2("priv/day16-PVZ2.txt").
% 64
% (aoc_2024@JuliusErisataT14.erisata.lt)6> day16:solve_2("priv/day16.txt").
% 518
% (aoc_2024@JuliusErisataT14.erisata.lt)7> timer:tc(fun() -> day16:solve_1("priv/day16.txt") end).
% {178849,85480}
% (aoc_2024@JuliusErisataT14.erisata.lt)8> timer:tc(fun() -> day16:solve_2("priv/day16.txt") end).
% {175811,518}


read_inputs(FileName) ->
    Lines = utils:read_lines_no_new_line(FileName),
    utils:get_char_matrix(Lines).


get_score(Direction, Direction) -> 1;
get_score(right,     down     ) -> 1001;
get_score(right,     left     ) -> undefined;
get_score(right,     up       ) -> 1001;
get_score(down,      left     ) -> 1001;
get_score(down,      up       ) -> undefined;
get_score(down,      right    ) -> 1001;
get_score(left,      up       ) -> 1001;
get_score(left,      right    ) -> undefined;
get_score(left,      down     ) -> 1001;
get_score(up,        right    ) -> 1001;
get_score(up,        down     ) -> undefined;
get_score(up,        left     ) -> 1001.


reverse(right) -> left;
reverse(down ) -> up;
reverse(left ) -> right;
reverse(up   ) -> down.


walk_map([], _, _, AccScores) -> AccScores;
walk_map([{Index,Direction,Score}|Tiles], Map, Dimensions, Scores) ->
    Directions = case Direction of
        right -> [right, down, up];
        down  -> [down, right, left];
        left  -> [left, down, up];
        up    -> [up, right, left]
    end,
    %Directions = [right, down, left, up] -- [reverse(Direction)],
    {NewTiles, NewScores} = lists:foldl(fun(NewDirection, {AccTiles, AccScores}) ->
        NewIndex = utils:matrix_next_index(Index, NewDirection, Dimensions),
        case NewIndex of
            undefined -> {AccTiles, AccScores};
            _ ->
                case maps:get(NewIndex, Map) of
                    $# -> {AccTiles, AccScores};
                    Char when Char =:= $.; Char =:= $E; Char =:= $S ->
                        NewScore = Score + get_score(Direction, NewDirection),
                        OldScores = maps:get(NewIndex, AccScores, #{}),
                        OldScore = maps:get(NewDirection, OldScores, infinity),
                        case NewScore < OldScore of
                            true  ->
                                NewAccTiles = AccTiles ++ [{NewIndex, NewDirection, NewScore}],
                                NewScores = OldScores#{NewDirection => NewScore},
                                NewAccScores = AccScores#{NewIndex => NewScores},
                                {NewAccTiles, NewAccScores};
                            false ->
                                {AccTiles, AccScores}
                        end
                end
        end
    end, {Tiles, Scores}, Directions),
    walk_map(NewTiles, Map, Dimensions, NewScores).


list_tiles([], _, _, AccOnPath) -> lists:usort(AccOnPath);
list_tiles([{Index,Direction,Score}|Indexes], Dimensions, Scores, AccOnPath) ->
    NextIndex = utils:matrix_next_index(Index, reverse(Direction), Dimensions),
    IndexScores = maps:get(NextIndex, Scores),
    %utils:print("XXX ~p ~p ~p", [Direction, Score, IndexScores]),
    NewIndexes = maps:fold(fun(NewDirection, NewScore, AccIndexes) ->
        case get_score(NewDirection, Direction) of
            undefined ->
                AccIndexes;
            MoveScore when MoveScore =:= Score-NewScore ->
                [{NextIndex, NewDirection, NewScore}|AccIndexes];
            _ ->
                AccIndexes
        end
    end, Indexes, IndexScores),
    % utils:print("XXX ~p -> ~p", [Index, NewIndexes]),
    % utils:wait_key_press(),
    list_tiles(NewIndexes, Dimensions, Scores, [Index|AccOnPath]).


solve_1(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    %utils:print_char_matrix(Map, Dimensions),
    Start = utils:matrix_index_of($S, Map),
    Scores = walk_map([{Start, right, 0}], Map, Dimensions, #{}),
    End = utils:matrix_index_of($E, Map),
    EndScores = maps:get(End, Scores),
    lists:min(maps:values(EndScores)).


solve_2(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    %utils:print_char_matrix(Map, Dimensions),
    Start = utils:matrix_index_of($S, Map),
    Scores = walk_map([{Start, right, 0}], Map, Dimensions, #{}),
    End = utils:matrix_index_of($E, Map),
    EndScores = maps:get(End, Scores),
    MinScore = lists:min(maps:values(EndScores)),
    Directions = [ Direction || {Direction, Score} <- maps:to_list(EndScores), Score =:= MinScore ],
    Ends = [{End, Direction, MinScore} || Direction <- Directions],
    erlang:length([Start|list_tiles(Ends, Dimensions, Scores, [])]).