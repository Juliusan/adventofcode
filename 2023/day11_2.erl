-module(day11_2).
-export([solve/2]).

% 00:03:17 - Nesudėtinga. Iš viso nėra ką veikti.
%
% 24> day11_2:solve("day11-PVZ.txt", 10).
% 1030
% 25> day11_2:solve("day11-PVZ.txt", 100).
% 8410
% 26> day11_2:solve("day11-IN.txt", 1000000).
% 504715068438

solve(FileName, Expansion) ->
    {ok, File} = file:open(FileName, [read]),
    GetMapFun = fun GetMapFun(AccMap) ->
        case file:read_line(File) of
            eof ->
                lists:reverse(AccMap);
            {ok, Line} ->
                %io:fwrite("XXX ~p: ", [Line]),
                MapLine = get_map_line(Line),
                %io:fwrite("~p~n", [MapNode]),
                GetMapFun([MapLine | AccMap])
        end
    end,
    Map = GetMapFun([]),
    ok = file:close(File),
    Galaxies = get_galaxies(Map),
    EmptyRows = get_empty_rows(Map),
    EmptyCols = get_empty_cols(Map),
    NewGalaxies = edit_galaxies(Galaxies, EmptyRows, EmptyCols, Expansion),
    Result = get_shortest_paths_sum(NewGalaxies),
    Result.
    
    
get_map_line(Line) ->
    trim_ending_newline(Line).
    
    
get_galaxies(Map) ->
    {_, Galaxies} = lists:foldl(fun(MapRow, {RowNum, AccGalaxies}) ->
        {_, Gs} = lists:foldl(fun
            ($., {ColNum, AGs}) -> {ColNum+1, AGs};
            ($#, {ColNum, AGs}) -> {ColNum+1, [{RowNum, ColNum}|AGs]}
        end, {1, AccGalaxies}, MapRow),
        {RowNum+1, Gs}
    end, {1, []}, Map),
    lists:reverse(Galaxies).
    
    
get_empty_rows(Map) ->
    {_, EmptyRows} = lists:foldl(fun(MapRow, {RowNum, AccEmptyRows}) ->
        case lists:all(fun(Point) -> Point =:= $. end, MapRow) of
            true  -> {RowNum+1, [RowNum | AccEmptyRows]};
            false -> {RowNum+1, AccEmptyRows}
        end
    end, {1, []}, Map),
    lists:reverse(EmptyRows).
    
    
get_empty_cols(Map) ->
    MapT = transpose(Map),
    get_empty_rows(MapT).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
  
  
edit_galaxies(Galaxies, EmptyRows, EmptyCols, Expansion) ->
    lists:map(fun({Row, Col}) ->
        ExpandFun = fun(In, List) ->
            Add = erlang:length(lists:filter(fun(Elem) -> Elem < In end, List)),
            In + Add*(Expansion-1)
        end,
        {ExpandFun(Row, EmptyRows), ExpandFun(Col, EmptyCols)}
    end, Galaxies).


get_shortest_paths_sum(Galaxies) ->
    get_shortest_paths_sum(Galaxies, 0).

get_shortest_paths_sum([_Galaxy], AccSum) ->
    AccSum;
    
get_shortest_paths_sum([Galaxy|Galaxies], AccSum) ->
    Sum = get_shortest_paths_sum(Galaxy, Galaxies),
    get_shortest_paths_sum(Galaxies, AccSum+Sum);
    
get_shortest_paths_sum({Row1, Col1}, Galaxies) ->
    lists:foldl(fun({Row2, Col2}, AccSum) ->
        Distance = erlang:abs(Row2-Row1) + erlang:abs(Col2-Col1),
        AccSum + Distance
    end, 0, Galaxies).
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

