-module(day12).
-export([solve_1/1, solve_2/1]).

% Pirmą užduotį darydamas iš karto nesupratau, kad tomis pačiomis raidėmis
% pažymėti nesiribojantys regionai iš tiesų yra skirtingi. Tai padariau
% vienaip, po to teko perdarinėti. Antrą dalį truputį užtruko, kol sugalvojau,
% kaip padaryti. Po to buvo tiesiog kodo rašymas ir debuginimas.

% (aoc_2024@JuliusErisataT14.erisata.lt)1> day12:solve_1("priv/day12-PVZ1.txt").
% 140
% (aoc_2024@JuliusErisataT14.erisata.lt)2> day12:solve_1("priv/day12-PVZ2.txt").
% 772
% (aoc_2024@JuliusErisataT14.erisata.lt)3> day12:solve_1("priv/day12-PVZ3.txt").
% 1930
% (aoc_2024@JuliusErisataT14.erisata.lt)4> day12:solve_1("priv/day12.txt").
% 1374934
% (aoc_2024@JuliusErisataT14.erisata.lt)5> day12:solve_2("priv/day12-PVZ1.txt").
% 80
% (aoc_2024@JuliusErisataT14.erisata.lt)6> day12:solve_2("priv/day12-PVZ4.txt").
% 236
% (aoc_2024@JuliusErisataT14.erisata.lt)7> day12:solve_2("priv/day12-PVZ5.txt").
% 368
% (aoc_2024@JuliusErisataT14.erisata.lt)8> day12:solve_2("priv/day12-PVZ3.txt").
% 1206
% (aoc_2024@JuliusErisataT14.erisata.lt)9> day12:solve_2("priv/day12.txt").
% 841078
% (aoc_2024@JuliusErisataT14.erisata.lt)10> timer:tc(fun() -> day12:solve_1("priv/day12.txt") end).
% {51508,1374934}
% (aoc_2024@JuliusErisataT14.erisata.lt)11> timer:tc(fun() -> day12:solve_2("priv/day12.txt") end).
% {65002,841078}


read_inputs(FileName) ->
    MapLines = lists:reverse(utils:read_lines_no_new_line(FileName)),
    utils:get_char_matrix(MapLines).


get_regions(Map, Dimensions) ->
    get_regions(maps:to_list(Map), 1, Map, Dimensions, #{}, #{}).


get_regions([], _, _, _, _, Acc) -> Acc;
get_regions([{Index,Name}|Indexes], Nr, Map, Dimensions, AccWalked, AccRegions) ->
    case maps:get(Index, AccWalked, undefined) of
        undefined ->
            {RegionIndexes, NewAccWalked} = get_region(Name, Index, Map, Dimensions, [], AccWalked),
            NewAccRegions = AccRegions#{{Nr, Name} => RegionIndexes},
            get_regions(Indexes, Nr+1, Map, Dimensions, NewAccWalked, NewAccRegions);
        true ->
            get_regions(Indexes, Nr, Map, Dimensions, AccWalked, AccRegions)
    end.


get_region(Name, Index, Map, Dimensions, AccRegion, AccWalked) ->
    case maps:get(Index, AccWalked, undefined) of
        undefined ->
            NextIndexes = lists:map(fun(Direction) -> utils:matrix_next_index(Index, Direction, Dimensions) end, [up, right, down, left]),
            lists:foldl(fun
                (undefined, {AccR, AccW}) -> {AccR, AccW};
                (NextIndex, {AccR, AccW}) ->
                    case maps:get(NextIndex, Map) of
                        Name -> get_region(Name, NextIndex, Map, Dimensions, AccR, AccW);
                        _    -> {AccR, AccW}
                    end
            end, {[Index|AccRegion], AccWalked#{Index => true}}, NextIndexes);
        true ->
            {AccRegion, AccWalked}
    end.


count_areas(Regions) ->
    maps:fold(fun(Region, Indexes, Acc) ->
        Acc#{Region => erlang:length(Indexes)}
    end, #{}, Regions).


count_perimeters(Regions, Map, Dimensions) ->
    maps:fold(fun({_, Name} = Region, Indexes, Acc) ->
        Perimeter = lists:foldl(fun(Index, Accc) ->
            NextIndexes = lists:map(fun(Direction) -> utils:matrix_next_index(Index, Direction, Dimensions) end, [up, right, down, left]),
            Count = lists:foldl(fun
                (undefined, Acccc) -> Acccc+1;
                (NextIndex, Acccc) ->
                    case maps:get(NextIndex, Map) of
                        Name -> Acccc;
                        _    -> Acccc+1
                    end
            end, 0, NextIndexes),
            %utils:print("XXX ~s ~p ~p", [[Elem], Index, Count]),
            Accc + Count
        end, 0, Indexes),
        Acc#{Region => Perimeter}
    end, #{}, Regions).


count_sides(Regions, Map, Dimensions) ->
    maps:fold(fun({_, Name} = Region, Indexes, Acc) ->
        {RowMapNS, ColMapNS} = lists:foldl(fun({Row, Col}, {AccRowMap, AccColMap}) ->
            NewAccRowMap = case maps:get(Row, AccRowMap, undefined) of
                undefined -> AccRowMap#{Row => [Col]};
                RIndexes  -> AccRowMap#{Row => [Col|RIndexes]}
            end,
            NewAccColMap = case maps:get(Col, AccColMap, undefined) of
                undefined -> AccColMap#{Col => [Row]};
                CIndexes  -> AccColMap#{Col => [Row|CIndexes]}
            end,
            {NewAccRowMap, NewAccColMap}
        end, {#{}, #{}}, Indexes),
        RowMap = maps:map(fun(_, List) -> lists:sort(List) end, RowMapNS),
        ColMap = maps:map(fun(_, List) -> lists:sort(List) end, ColMapNS),
        CountSidesFun = fun(Dim1, Dim2s, MakeIndexFun, SuccDirection, NextDirection) ->
            {_, _, Count} = lists:foldl(fun(Dim2, {PrevBorder, ExpectedIndex, AccCount}) ->
                Index = MakeIndexFun(Dim1, Dim2),
                SuccIndex = utils:matrix_next_index(Index, SuccDirection, Dimensions),
                IsBorder = case SuccIndex of
                    undefined ->
                        true;
                    _ ->
                        case maps:get(SuccIndex, Map) of
                            Name      -> false;
                            _         -> true
                    end
                end,
                NewAccCount = case {Index =:= ExpectedIndex, IsBorder, PrevBorder} of
                    {true,  true, false    } -> AccCount+1;
                    {false, true,  _       } -> AccCount+1;
                    {_,     _,     _       } -> AccCount
                end,
                NextIndex = utils:matrix_next_index(Index, NextDirection, Dimensions),
                {IsBorder, NextIndex, NewAccCount}
            end, {false, MakeIndexFun(Dim1, 1), 0}, Dim2s),
            %utils:print("XXX region ~p dimension ~p-~p (~p) count ~p", [[Name], Dim1, SuccDirection, Dim2s, Count]),
            Count
        end,
        RowCount = maps:fold(fun(Row, RowCols, AccCount) ->
            CountFun = fun(Dir) -> CountSidesFun(Row, RowCols, fun(D1, D2) -> {D1, D2} end, Dir, right) end,
            Count1 = CountFun(up  ),
            Count2 = CountFun(down),
            AccCount + Count1 + Count2
        end, 0, RowMap),
        ColCount = maps:fold(fun(Col, ColRows, AccCount) ->
            CountFun = fun(Dir) -> CountSidesFun(Col, ColRows, fun(D1, D2) -> {D2, D1} end, Dir, down) end,
            Count1 = CountFun(right),
            Count2 = CountFun(left),
            AccCount + Count1 + Count2
        end, 0, ColMap),
        Acc#{Region => RowCount + ColCount}
    end, #{}, Regions).


count_price(Areas, Perimeters) ->
    maps:fold(fun({Nr, Name}=Elem, Area, Acc) ->
        Perimeter = maps:get(Elem, Perimeters),
        %utils:print("Plot ~p-~s area ~p perimeter ~p", [Nr, [Name], Area, Perimeter]),
        Price = Area*Perimeter,
        Acc+Price
    end, 0, Areas).


solve_1(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    Regions = get_regions(Map, Dimensions),
    Areas = count_areas(Regions),
    Perimeters = count_perimeters(Regions, Map, Dimensions),
    count_price(Areas, Perimeters).


solve_2(FileName) ->
    {Map, Dimensions} = read_inputs(FileName),
    Regions = get_regions(Map, Dimensions),
    Areas = count_areas(Regions),
    Sides = count_sides(Regions, Map, Dimensions),
    count_price(Areas, Sides).
