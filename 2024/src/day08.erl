-module(day08).
-export([solve_1/1, solve_2/1]).

% Ilgokai užtruko, kol parašiau pirmą dalį. Patį algoritmą sudėliojau galvoje
% greitai. Užtruko užrašymas. Problemų neturėjau. Antrai daliai nebuvo laiko,
% bet kai Rimas parašė, kad nesunku, tai prisėdau greičiau, nei planavau ir
% iš tiesų greitai padariau.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day08:solve_1("priv/day08-PVZ1.txt").
% 2
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day08:solve_1("priv/day08-PVZ2.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day08:solve_1("priv/day08-PVZ3.txt").
% 4
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day08:solve_1("priv/day08-PVZ4.txt").
% 14
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day08:solve_1("priv/day08.txt").
% 413
% (aoc_2024@JuliusErisataT14.erisata.lt)6> day08:solve_2("priv/day08-PVZ5.txt").
% 9
% (aoc_2024@JuliusErisataT14.erisata.lt)7> day08:solve_2("priv/day08-PVZ4.txt").
% 34
% (aoc_2024@JuliusErisataT14.erisata.lt)8> day08:solve_2("priv/day08.txt").
% 1417


read_inputs(FileName) ->
    MapLines = lists:reverse(utils:read_lines_no_new_line(FileName)),
    {Matrix, Dimensions} = utils:get_char_matrix(MapLines),
    Antennas = utils:matrix_foldl(fun
        (_Index, $.,        Acc) -> Acc;
        ( Index, Frequency, Acc) ->
            case maps:get(Frequency, Acc, undefined) of
                undefined -> Acc#{Frequency => [Index]};
                Indices   -> Acc#{Frequency => [Index|Indices]}
            end
    end, #{}, Matrix, Dimensions),
    {Antennas, Dimensions}.


count_frequency_antinodes(Indices, Dimensions, CountMany) ->
    utils:foldl_pairs(fun({Row1, Col1} = Index1, {Row2, Col2} = Index2, Acc) ->
        RowDif = Row2-Row1,
        ColDif = Col2-Col1,
        Antinodes = case CountMany of
            false ->
                PossibleAntinodes = [{Row1-RowDif, Col1-ColDif}, {Row2+RowDif, Col2+ColDif}],
                lists:filter(fun(Index) -> utils:matrix_is_valid_index(Index, Dimensions) end, PossibleAntinodes);
            true ->
                GetAntinodesFun = fun GetAntinodesFun({RowC, ColC} = IndexC, IndexFun, Accc) ->
                    case utils:matrix_is_valid_index(IndexC, Dimensions) of
                        true ->
                            RowN = IndexFun(RowC, RowDif),
                            ColN = IndexFun(ColC, ColDif),
                            GetAntinodesFun({RowN, ColN}, IndexFun, [IndexC | Accc]);
                        false ->
                            Accc
                    end
                end,
                BackAntinodes = GetAntinodesFun(Index1, fun(I, D) -> I - D end, []),
                FwdAntinodes  = GetAntinodesFun(Index2, fun(I, D) -> I + D end, []),
                BackAntinodes ++ FwdAntinodes
        end,
        Antinodes ++ Acc
    end, [], Indices).


count_antinodes(Antennas, Dimensions, CountMany) ->
    AllAntinodes = maps:fold(fun(_Frequency, Indices, Acc) ->
        Antinodes = count_frequency_antinodes(Indices, Dimensions, CountMany),
        Antinodes ++ Acc
    end, [], Antennas),
    erlang:length(lists:usort(AllAntinodes)).



solve_1(FileName) ->
    {Antennas, Dimensions} = read_inputs(FileName),
    count_antinodes(Antennas, Dimensions, false).


solve_2(FileName) ->
    {Antennas, Dimensions} = read_inputs(FileName),
    count_antinodes(Antennas, Dimensions, true).