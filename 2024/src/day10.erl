-module(day10).
-export([solve_1/1, solve_2/1]).

% Nesudėtingos abi dalys. Labai ilgai šunkeliais nevaikščiojau ir iš karto
% parašiau gerą variantą. Tik vis tiek užtrunka, kol sudedi visą logiką.
% Žinoma, labai padėjo visokios utility funkcijos. Šiaip net nežinau, ką čia
% dar patvarkyti. Kodas geras iš pirmo karto.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day10:solve_1("priv/day10-PVZ1.txt").
% 1
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day10:solve_1("priv/day10-PVZ2.txt").
% 2
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day10:solve_1("priv/day10-PVZ3.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day10:solve_1("priv/day10-PVZ4.txt").
% 3
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day10:solve_1("priv/day10-PVZ5.txt").
% 36
% (aoc_2024@JuliusErisataT14.erisata.lt)6> day10:solve_1("priv/day10.txt").
% 694
% (aoc_2024@JuliusErisataT14.erisata.lt)7> day10:solve_2("priv/day10-PVZ6.txt").
% 3
% (aoc_2024@JuliusErisataT14.erisata.lt)8> day10:solve_2("priv/day10-PVZ7.txt").
% 13
% (aoc_2024@JuliusErisataT14.erisata.lt)9> day10:solve_2("priv/day10-PVZ8.txt").
% 227
% (aoc_2024@JuliusErisataT14.erisata.lt)10> day10:solve_2("priv/day10-PVZ5.txt").
% 81
% (aoc_2024@JuliusErisataT14.erisata.lt)11> day10:solve_2("priv/day10.txt").
% 1497
% (aoc_2024@JuliusErisataT14.erisata.lt)12> timer:tc(fun() -> day10:solve_1("priv/day10.txt") end).
% {11580,694}
% (aoc_2024@JuliusErisataT14.erisata.lt)13> timer:tc(fun() -> day10:solve_2("priv/day10.txt") end).
% {15698,1497}


read_inputs(FileName) ->
    MapLines = lists:reverse(utils:read_lines_no_new_line(FileName)),
    utils:get_char_matrix(MapLines).


walk_the_map(Map, Dimensions) ->
    utils:matrix_foldl(fun
        (Index,$0,Acc) ->
            NinesVisited = walk_the_map(Index, Map, Dimensions),
            [NinesVisited|Acc];
        (_Index,_,Acc) ->
            Acc
    end, [], Map, Dimensions).


walk_the_map(Index, Map, Dimensions) ->
    walk_the_map(Index, $0, Map, Dimensions, [], #{}).

walk_the_map(Index, Height, Map, Dimensions, AccNines, AccVisited) ->
    lists:foldl(fun(Direction, {AccN, AccV}) ->
        %utils:print("~p ~p, ~p, ~p", [Index, Height, AccN, AccV]),
        case utils:matrix_next_index(Index, Direction, Dimensions) of
            undefined ->
                {AccN, AccV};
            NextIndex ->
                case maps:get(NextIndex, Map) of
                    NextHeight when NextHeight =:= Height+1 ->
                        VisitedFrom = maps:get(NextIndex, AccV, []),
                        NewAccV = AccV#{NextIndex => lists:usort([Index|VisitedFrom])},
                        case {VisitedFrom, NextHeight} of
                            {[],    $9} -> {[NextIndex|AccN], NewAccV};
                            {[],    _ } -> walk_the_map(NextIndex, NextHeight, Map, Dimensions, AccN, NewAccV);
                            {[_|_], _ } -> {AccN, NewAccV}
                        end;
                    _ ->
                        {AccN, AccV}
                end
        end
    end, {AccNines, AccVisited}, [left, down, right, up]).


count_nines(Nines, VisitedMap) ->
    count_nines(Nines, VisitedMap, 0).

count_nines([], _, Acc) ->
    Acc;

count_nines([Nine|Nines], VisitedMap, Acc) ->
    WaysToReach = maps:get(Nine, count_nine(Nine, 9, VisitedMap, #{})),
    count_nines(Nines, VisitedMap, Acc+WaysToReach).


count_nine(Index, 1, _, Acc) ->
    Acc#{Index => 1};

count_nine(Index, Nr, VisitedMap, Acc) ->
    case maps:get(Index, Acc, undefined) of
        undefined ->
            case maps:get(Index, VisitedMap, []) of
                [] ->
                    Acc#{Index => 0};
                [_|_] = VisitedFrom ->
                    NewAcc = lists:foldl(fun(I, Accc) ->
                        count_nine(I, Nr-1, VisitedMap, Accc)
                    end, Acc, VisitedFrom),
                    NewAcc#{Index => lists:sum(lists:map(fun(I) -> maps:get(I, NewAcc) end, VisitedFrom))}
            end;
        _ ->
            Acc
    end.


solve_1(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    NinesVisited = walk_the_map(Map, Dimensions),
    lists:sum(lists:map(fun({Nines,_}) -> erlang:length(Nines) end, NinesVisited)).


solve_2(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    NinesVisited = walk_the_map(Map, Dimensions),
    lists:sum(lists:map(fun({Nines, VisitedMap}) -> count_nines(Nines, VisitedMap) end, NinesVisited)).
