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
            lists:foldl(fun(NextIndex, {AccR, AccW}) ->
                case is_tile_suitable(NextIndex, Name, Map) of
                    true -> get_region(Name, NextIndex, Map, Dimensions, AccR, AccW);
                    _    -> {AccR, AccW}
                end
            end, {[Index|AccRegion], AccWalked#{Index => true}}, NextIndexes);
        true ->
            {AccRegion, AccWalked}
    end.


is_tile_suitable(undefined, _   , _  ) -> false;
is_tile_suitable(Index,     Name, Map) ->
    case maps:get(Index, Map) of
        Name -> true;
        _    -> false
    end.


count_areas(Regions) ->
    maps:map(fun(_Region, Indexes) ->
        erlang:length(Indexes)
    end, Regions).


count_perimeters(Regions, Map, Dimensions) ->
    maps:map(fun({_, Name}, Indexes) ->
        utils:list_map_sum(fun(Index) ->
            NextIndexes = lists:map(fun(Direction) -> utils:matrix_next_index(Index, Direction, Dimensions) end, [up, right, down, left]),
            utils:list_filter_count(fun(NextIndex) ->
                not(is_tile_suitable(NextIndex, Name, Map))
            end, NextIndexes)
        end, Indexes)
    end, Regions).


count_sides(Regions, Map, Dimensions) ->
    maps:map(fun({_, Name}, Indexes) ->
        {RowMapNS, ColMapNS} = lists:foldl(fun({Row, Col}, {AccRowMap, AccColMap}) ->
            NewAccRowMap = maps:update_with(Row, fun(RCols) -> [Col|RCols] end, [Col], AccRowMap),
            NewAccColMap = maps:update_with(Col, fun(CRows) -> [Row|CRows] end, [Row], AccColMap),
            {NewAccRowMap, NewAccColMap}
        end, {#{}, #{}}, Indexes),
        SortValuesFun = fun(_, List) -> lists:sort(List) end,
        RowMap = maps:map(SortValuesFun, RowMapNS),
        ColMap = maps:map(SortValuesFun, ColMapNS),
        CountSidesFun = fun(Dim1, Dim2s, MakeIndexFun, SuccDirection, NextDirection) ->
            {Count, _} = utils:list_foldl_count(fun(Dim2, {PrevBorder, ExpectedIndex}) ->
                Index = MakeIndexFun(Dim1, Dim2),
                SuccIndex = utils:matrix_next_index(Index, SuccDirection, Dimensions),
                IsBorder = not(is_tile_suitable(SuccIndex, Name, Map)),
                DoCount = case {Index =:= ExpectedIndex, IsBorder, PrevBorder} of
                    {true,  true, false} -> true;
                    {false, true,  _   } -> true;
                    {_,     _,     _   } -> false
                end,
                NextIndex = utils:matrix_next_index(Index, NextDirection, Dimensions),
                {DoCount, {IsBorder, NextIndex}}
            end, {false, MakeIndexFun(Dim1, 1)}, Dim2s),
            %utils:print("XXX region ~p dimension ~p-~p (~p) count ~p", [[Name], Dim1, SuccDirection, Dim2s, Count]),
            Count
        end,
        RowCount = utils:map_map_sum(fun(Row, RowCols) ->
            CountFun = fun(Dir) -> CountSidesFun(Row, RowCols, fun(D1, D2) -> {D1, D2} end, Dir, right) end,
            Count1 = CountFun(up  ),
            Count2 = CountFun(down),
            Count1 + Count2
        end, RowMap),
        ColCount = utils:map_map_sum(fun(Col, ColRows) ->
            CountFun = fun(Dir) -> CountSidesFun(Col, ColRows, fun(D1, D2) -> {D2, D1} end, Dir, down) end,
            Count1 = CountFun(right),
            Count2 = CountFun(left),
            Count1 + Count2
        end, ColMap),
        RowCount + ColCount
    end, Regions).


count_price(Areas, Lengths) ->
    utils:map_map_sum(fun(Elem, Area) ->            % {Nr, Name}=Elem
        Length = maps:get(Elem, Lengths),
        %utils:print("Plot ~p-~s area ~p perimeter/side ~p", [Nr, [Name], Area, Length]),
        Area*Length
    end, Areas).


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
