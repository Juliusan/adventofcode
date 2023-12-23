-module(day23_1).
-export([solve/1]).

% Nesudėtinga. Primityvus būdas suveikė. Reikėjo tik suprogramuoti

solve(FileName) ->
    Map = get_map(FileName),
    {MapE, Size} = encode_map(Map),
    %io:fwrite("XXX ~p~n", [Size]),
    Result = traverse_and_count(MapE, Size),
    Result.
    
    
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
    
    
encode_map(Map) ->
    {Rows1, MapE} = lists:foldl(fun(MapLine, {Row, AccMapE}) ->
        {_, NewMapE} = lists:foldl(fun
            (Tile, {Column, AccME}) ->
                {Column+1, AccME#{Column => Tile}}
        end, {1, #{}}, MapLine),
        {Row+1, AccMapE#{Row => NewMapE}}
    end, {1, #{}}, Map),
    [First|_] = Map,
    Rows = Rows1-1,
    Cols = erlang:length(First),
    {MapE, {Rows, Cols}}.


traverse_and_count(Map, MapSize) ->
    Length = traverse_map(Map, MapSize, {1,2}, {2,2}, false, 1, []),
    Length.

    
traverse_map(_Map, {Rows, Cols}, _, {Rows, Col}, _Slope, Length, _) when Col =:= Cols-1 ->
    %io:fwrite("XXX EUREKA! ~p~n", [Length]),
    Length;
    
traverse_map(Map, MapSize, From, To, Slope, Length, Visited) ->
    %io:fwrite("XXX TRAVERSE ~p ~p ~p ~p ~p~n", [From, To, Length, Slope, Visited]),
    NextCells = get_next_cells(Map, MapSize, To, Slope, [From|Visited]),
    case NextCells of
        []              -> false;
        [{Next, true }] -> traverse_map(Map, MapSize, To, Next, not(Slope), Length+1, Visited);
        [{Next, false}] -> traverse_map(Map, MapSize, To, Next, Slope, Length+1, Visited);
        [_,_|_] = Nexts ->
            lists:foldl(fun({Next, IsSlope}, AccLength) ->
                NextSlope = case IsSlope of
                    true  -> not(Slope);
                    false -> Slope
                end,
                NextLength = traverse_map(Map, MapSize, To, Next, NextSlope, Length+1, [To|Visited]),
                case {NextLength, AccLength} of
                    {_, false}        -> NextLength;
                    {false, _}        -> AccLength;
                    {L, A} when L > A -> NextLength;
                    _                 -> AccLength
                end
            end, false, Nexts)
    end.
    
    
get_next_cells(Map, {Rows, Cols}, {Row, Col}, Slope, Visited) ->
    Right = case Col =:= Cols of
        true  -> [];
        false -> [{$>, {Row, Col+1}}]
    end,
    Down = case Row =:= Rows of
        true  -> [];
        false -> [{$v, {Row+1, Col}}]
    end,
    Left = case Col of
        1 -> [];
        _ -> [{$<, {Row, Col-1}}]
    end,
    Up = case Row of
        1 -> [];
        _ -> [{$^, {Row-1, Col}}]
    end,
    AllCases = Right ++ Down ++ Left ++ Up,
    lists:filtermap(fun({GoodSlope, Next}) ->
        case lists:member(Next, Visited) of
            true ->
                false;
            false ->
                case {get_cell(Next, Map), Slope} of
                    {$#,        _    } -> false;
                    {$.,        _    } -> {true, {Next, false}};
                    {$>,        false} -> {true, {Next, true}};
                    {$v,        false} -> {true, {Next, true}};
                    {$<,        false} -> {true, {Next, true}};
                    {$^,        false} -> {true, {Next, true}};
                    {GoodSlope, true } -> {true, {Next, true}};
                    {_        , true } -> false
                end
        end
    end, AllCases).
    
    
get_cell({Row, Col}, Map) ->
    #{Row := RowMap} = Map,
    #{Col := Tile} = RowMap,
    Tile.


trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

