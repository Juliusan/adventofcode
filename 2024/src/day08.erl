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
    {Matrix, Rows, Cols} = utils:get_char_matrix(MapLines),
    Antenas = utils:matrix_foldl(fun
        (_Row, _Column, $., Acc) -> Acc;
        (Row, Column, Value, Acc) ->
            case maps:get(Value, Acc, undefined) of
                undefined -> Acc#{Value => [{Row, Column}]};
                Locations -> Acc#{Value => [{Row, Column}|Locations]}
            end
    end, #{}, Matrix, Rows, Cols),
    {Antenas, Rows, Cols}.


count_antena_antinodes(Locations, Rows, Cols, CountFun) ->
    count_antena_antinodes(Locations, Rows, Cols, [], CountFun).


count_antena_antinodes([_], _, _, Acc, _) -> Acc;
count_antena_antinodes([Location|Locations], Rows, Cols, Acc, CountFun) ->
    NewAntinodes = CountFun(Location, Locations, Rows, Cols, Acc),
    count_antena_antinodes(Locations, Rows, Cols, NewAntinodes ++ Acc, CountFun).


count_antena_antinodes1(_, [], _, _, Acc) ->
    Acc;

count_antena_antinodes1({Row1,Col1}, [{Row2, Col2}|Locations], Rows, Cols, Acc) ->
    RowDif = Row2-Row1,
    ColDif = Col2-Col1,
    Antena1OnMap = location_on_map(Row1-RowDif, Col1-ColDif, Rows, Cols),
    Antena2OnMap = location_on_map(Row2+RowDif, Col2+ColDif, Rows, Cols),
    %utils:print("NewANTI: ~p ~p -> ~p ~p", [{Row1,Col1}, {Row2, Col2}, {Row1-RowDif, Col1-ColDif}, Antena1OnMap]),
    %utils:print("NewANTI: ~p ~p -> ~p ~p", [{Row1,Col1}, {Row2, Col2}, {Row2+RowDif, Col2+ColDif}, Antena2OnMap]),
    NewAntinodes = case {Antena1OnMap, Antena2OnMap} of
        {false, false} -> [];
        {true,  false} -> [{Row1-RowDif, Col1-ColDif}];
        {false, true } -> [{Row2+RowDif, Col2+ColDif}];
        {true,  true } -> [{Row1-RowDif, Col1-ColDif}, {Row2+RowDif, Col2+ColDif}]
    end,
    count_antena_antinodes1({Row1,Col1}, Locations, Rows, Cols, NewAntinodes ++ Acc).


count_antena_antinodes2(_, [], _, _, Acc) ->
    Acc;

count_antena_antinodes2({Row1,Col1}, [{Row2, Col2}|Locations], Rows, Cols, Acc) ->
    RowDif = Row2-Row1,
    ColDif = Col2-Col1,
    FindFun = fun FindFun(RowC, ColC, IndexFun, Accc) ->
        RowN = IndexFun(RowC, RowDif),
        ColN = IndexFun(ColC, ColDif),
        case location_on_map(RowN, ColN, Rows, Cols) of
            true  -> FindFun(RowN, ColN, IndexFun, [{RowN, ColN} | Accc]);
            false -> Accc
        end
    end,
    BackAntenas = FindFun(Row1, Col1, fun(I, D) -> I - D end, []),
    FwdAntenas  = FindFun(Row2, Col2, fun(I, D) -> I + D end, []),
    count_antena_antinodes2({Row1,Col1}, Locations, Rows, Cols, [{Row1,Col1}, {Row2, Col2}] ++ BackAntenas ++ FwdAntenas ++ Acc).


location_on_map( Row, _Col, _Rows, _Cols) when Row<1    -> false;
location_on_map( Row, _Col,  Rows, _Cols) when Row>Rows -> false;
location_on_map(_Row,  Col, _Rows, _Cols) when Col<1    -> false;
location_on_map(_Row,  Col, _Rows,  Cols) when Col>Cols -> false;
location_on_map(_Row, _Col, _Rows, _Cols)               -> true.


count_antinodes(Antenas, Rows, Cols, CountFun) ->
    AllAntinodes = maps:fold(fun(Antena, Locations, Acc) ->
        Antinodes = lists:usort(count_antena_antinodes(Locations, Rows, Cols, CountFun)),
        %utils:print("ANTI: ~p -> ~p", [Antena, Antinodes]),
        Acc ++ Antinodes
    end, [], Antenas),
    erlang:length(lists:usort(AllAntinodes)).



solve_1(FileName) ->
    {Antenas, Rows, Cols} = read_inputs(FileName),
    %utils:print("~p", [Antenas]),
    count_antinodes(Antenas, Rows, Cols, fun count_antena_antinodes1/5).


solve_2(FileName) ->
    {Antenas, Rows, Cols} = read_inputs(FileName),
    %utils:print("~p", [Antenas]),
    count_antinodes(Antenas, Rows, Cols, fun count_antena_antinodes2/5).