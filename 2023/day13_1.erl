-module(day13_1).
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
    NewMapR  = lists:reverse(NewMap),
    NewMapTR = transpose(NewMapR),
    NewMapT  = lists:reverse(NewMapTR),
    Length   = erlang:length(NewMap),
    LengthT  = erlang:length(NewMapT),
    Maps = {{NewMapR, NewMap, Length}, {NewMapTR, NewMapT, LengthT}},
    NewAccMaps = [Maps | AccMaps],
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
    Result = count_result(Map),
    count_result_maps(Maps, Result + AccResult).
    
    
count_result({{[Line1|Map], _MapR, _Length} = Maps, {[Column1|MapT], _MapTR, _LengthT} = MapsT}) ->
    case find_symetry(Map, Line1, 1, Maps) of
        false ->
            case find_symetry(MapT, Column1, 1, MapsT) of
                false  -> error;
                CountT -> CountT
            end;
        Count ->
            Count*100
    end.

    
find_symetry([], _, _, _) ->
    false;
    
find_symetry([Line1|Map], Line0, Index, MapData) ->
    case lines_equal(Line0, Line1) of
        true ->
            case is_symetry(Index, MapData) of
                true  -> Index;
                false -> find_symetry(Map, Line1, Index+1, MapData)
            end;
        false ->
            find_symetry(Map, Line1, Index+1, MapData)
    end.
    
    
lines_equal(A, B) -> A=:=B.


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


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
    

skip_first(0,          Remaining ) -> Remaining;
skip_first(Count, [_ | Remaining]) -> skip_first(Count-1, Remaining).


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

