-module(day14_1).
-export([solve/1]).

% Nesudėtinga. Tik programavimas.

solve(FileName) ->
    Map = get_map(FileName),
    Weight = tilt_and_count(Map),
    Weight.
    
    
get_map(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Map = get_map(File, []),
    ok = file:close(File),
    Map.
    
get_map(File, AccMap) ->
    case file:read_line(File) of
        eof ->
            lists:reverse(AccMap);
        {ok, Line} ->
            %io:fwrite("XXX ~p: ", [Line]),
            MapLine = trim_ending_newline(Line),
            %io:fwrite("~p~n", [MapNode]),
            get_map(File, [MapLine | AccMap])
    end.
    
    
tilt_and_count(Map) ->
    MapT = transpose(Map),
    lists:foldl(fun(Line, AccSum) ->
        Sum = count(Line),
        Sum + AccSum
    end, 0, MapT).
    
    
count(Line) ->
    Length = erlang:length(Line),
    count(Line, Length, Length, 0).
    
count("",        _StoneW, _PosW, AccSum) -> AccSum;
count([$O|Line],  StoneW,  PosW, AccSum) -> count(Line, StoneW-1, PosW-1, AccSum+StoneW);
count([$.|Line],  StoneW,  PosW, AccSum) -> count(Line, StoneW,   PosW-1, AccSum); 
count([$#|Line], _StoneW,  PosW, AccSum) -> count(Line, PosW-1,   PosW-1, AccSum). 


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

