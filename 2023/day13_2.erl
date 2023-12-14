-module(day13_2).
-export([solve/1]).

% Nesudėtinga. Tik programavimas.

solve(FileName) ->
    Maps = get_maps(FileName),
    Result = count_result_maps(Maps),
    Result.
    
    
get_maps(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Maps = get_maps(File, []),
    ok = file:close(File),
    Maps.
    
get_maps(File, AccMaps) ->
    {NewMap, IsEOF} = get_map(File, []),
    NewAccMaps = [lists:reverse(NewMap) | AccMaps],
    case IsEOF of
        true  -> NewAccMaps;
        false -> get_maps(File, NewAccMaps)
    end.


get_map(File, AccMap) ->
    case file:read_line(File) of
        eof ->
            {AccMap, true};
        {ok, "\n"} ->
            {AccMap, false};
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            MapLine = trim_ending_newline(Line),
            %io:fwrite("~p~n", [MapNode]),
            get_map(File, [MapLine | AccMap])
    end.


count_result_maps(Maps) ->
    count_result_maps(Maps, 0).
    
count_result_maps([], AccResult) ->
    AccResult;
    
count_result_maps([Map | Maps], AccResult) ->
    %io:fwrite("VVVVVVVVVVVVVVVVVV~n"),
    %print_map(Map),
    %io:fwrite("AAAAAAAAAAAAAAAAAA~n"),
    Result = count_result(Map),
    count_result_maps(Maps, Result + AccResult).
    
    
count_result(Map) ->
    OrigCount = count_single(Map),
    case count_result_with_fix(Map, 0, Map, OrigCount, false) of
        false ->
            MapT = transpose(Map),
            count_result_with_fix(MapT, 0, MapT, OrigCount, true);
        Count ->
            Count
    end.

    
count_result_with_fix([_], _Index, _AllMap, _OrigCount, false) ->
    false;

count_result_with_fix([Line1, Second | Map], Index, AllMap, OrigCount, IsTransposed) ->
    case count_result_with_fix(Line1, [Second|Map], Index, Index+1, AllMap, OrigCount, IsTransposed) of
        false -> count_result_with_fix([Second|Map], Index+1, AllMap, OrigCount, IsTransposed);
        Count -> Count
    end.

count_result_with_fix(_Line1, [], _Index1, _Index2, _AllMap, _OrigCount, _IsTransposed) ->
    io:fwrite("XXX ~p ~p EMPTY~n", [_Index1, _Index2]),
    false;
    
count_result_with_fix(Line1, [Line2 | Map], Index1, Index2, AllMap, OrigCount, IsTransposed) ->
    io:fwrite("XXX ~p ~p~n", [Index1, Index2]),
    case fix_line(Line1, Line2) of
        false ->
            count_result_with_fix(Line1, Map, Index1, Index2+1, AllMap, OrigCount, IsTransposed); 
        undefined ->
            count_result_with_fix(Line1, Map, Index1, Index2+1, AllMap, OrigCount, IsTransposed); 
        {NewLine1, NewLine2} ->
            ResultFun = fun
                (false)     -> 0;
                (OrigCount) -> 0;
                (Result)    -> Result
            end,
            ResultRow    = ResultFun(replace_and_count(NewLine1, Index1, AllMap, IsTransposed)),
            ResultColumn = ResultFun(replace_and_count(NewLine2, Index2, AllMap, IsTransposed)),
            case {ResultRow, ResultColumn} of
                {0, 0} -> false;
                {_, _} -> ResultRow + ResultColumn
            end,
            case Result of
                false                  -> false;
                C when C =:= OrigCount -> io:fwrite("XXX WOW~n"), false;
                _                      -> Result
            end
    end.
            

replace_and_count(NewLine, Index, Map, IsTransposed) ->
    io:fwrite("XXXR ~p ~p ~p~n", [NewLine, Index, IsTransposed]),
    {First, [_|Last]} = split_by_count(Index, Map),
    NewMap  = First ++ [NewLine] ++ Last,
    %print_map(Map),
    %io:fwrite("..................~n"),
    %print_map(NewMap),
    %io:fwrite("------------------~n"),
    {Map1, Map2} = case IsTransposed of
        false -> {NewMap, transpose(NewMap)};
        true  -> {transpose(NewMap), NewMap}
    end,
    case count_single(Map1) of
        false -> count_single(Map2);
        Count -> Count*100
    end.
    
    
count_single(Map) ->
    MapR = lists:reverse(Map),
    Length = erlang:length(Map),
    [Line1 | OtherMap] = Map,
    find_symetry(OtherMap, Line1, 1, {Map, MapR, Length}).

    
find_symetry([], _, _, _) ->
    false;
    
find_symetry([Line1|Map], Line0, Index, MapData) ->
    case lines_equal(Line0, Line1) of
        true ->
            %io:fwrite("XXX EQUAL ~p ~p~n", [Index, is_symetry(Index, MapData)]),
            case is_symetry(Index, MapData) of
                true  -> Index;
                false -> find_symetry(Map, Line1, Index+1, MapData)
            end;
        false ->
            find_symetry(Map, Line1, Index+1, MapData)
    end.
    
    
lines_equal(A, B) -> A=:=B.


fix_line(Line1, Line2) ->
    fix_line(Line1, Line2, {"", "", 0}).

fix_line([], [], {_AccLine1, _AccLine2, 0}) -> false;

fix_line([], [], {AccLine1, AccLine2, 1}) -> {lists:reverse(AccLine1), lists:reverse(AccLine2)};

fix_line([A|Line1], [A|Line2], {AccLine1, AccLine2, Count}) ->
    fix_line(Line1, Line2, {[A|AccLine1], [A|AccLine2], Count});

fix_line([A|Line1], [B|Line2], {AccLine1, AccLine2, 0}) when A =/= B ->
    fix_line(Line1, Line2, {[B|AccLine1], [A|AccLine2], 1});
    
fix_line([A|_Line1], [B|_Line2], {_AccLine, _AccLine2, 1}) when A =/= B ->
    false.


is_symetry(Index, {Map, MapR, Length}) ->
    LinesBelow = Length - Index,
    LinesAbove = Index,
    {Map1, Map2, Count} = case LinesAbove > LinesBelow of
        true  -> {skip_first(LinesAbove-LinesBelow, Map), MapR,                                      LinesBelow};
        false -> {                                  Map , skip_first(LinesBelow - LinesAbove, MapR), LinesAbove}
    end,
    compare(Map1, Map2, Count).
    
    
compare(_, _, 0) -> true;    

compare([Line1|Map1], [Line2|Map2], Count) ->
    case lines_equal(Line1, Line2) of
        true  -> compare(Map1, Map2, Count-1);
        false -> false
    end.


print_map([]) -> ok;
print_map([S|OtherRows]) ->
    io:fwrite("Map ~p~n", [S]),
    print_map(OtherRows).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
    

skip_first(Count, List) ->
    {_, Remaining} = split_by_count(Count, List),
    Remaining.


split_by_count(Count, List) ->
    split_by_count(Count, List, []).

split_by_count(0,          Remaining,  First) -> {lists:reverse(First), Remaining};
split_by_count(Count, [E | Remaining], First) -> split_by_count(Count-1, Remaining, [E | First]).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

