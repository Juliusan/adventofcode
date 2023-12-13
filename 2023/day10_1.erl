-module(day10_1).
-export([solve/1]).

% Nesudėtinga. Tik programavimas.

solve(FileName) ->
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
    Start = get_start_tile(Map),
    LoopLength = get_loop_length(Start, Map),
    Furtherst = get_furtherst(LoopLength),
    Furtherst.
    
    
get_map_line(Line) ->
    trim_ending_newline(Line).


get_start_tile(Map) ->
    get_start_tile({1, 1}, Map).
    
get_start_tile(Current,    [[$S|_       ]|_        ]) -> Current;
get_start_tile({Row, Col}, [[_|OtherCols]|OtherRows]) -> get_start_tile({Row, Col+1}, [OtherCols | OtherRows]);
get_start_tile({Row, _  }, [[]           |OtherRows]) -> get_start_tile({Row+1, 1  }, OtherRows).


is_tile_ok_for_start(_,       {0,_}, _    )                      -> false;
is_tile_ok_for_start(_,       {_,0}, _    )                      -> false;
is_tile_ok_for_start(_,       {R,_}, Map  ) when R > length(Map) -> false;
is_tile_ok_for_start(_,       {_,C}, [R|_]) when C > length(R)   -> false;
is_tile_ok_for_start(right,   Index, Map  )                      -> is_tile_ok_for_start([$J, $-, $7], Index, Map);
is_tile_ok_for_start(bottom,  Index, Map  )                      -> is_tile_ok_for_start([$J, $|, $L], Index, Map);
is_tile_ok_for_start(left,    Index, Map  )                      -> is_tile_ok_for_start([$L, $-, $F], Index, Map);
is_tile_ok_for_start(top,     Index, Map  )                      -> is_tile_ok_for_start([$7, $|, $F], Index, Map);
is_tile_ok_for_start(OKTiles, Index, Map  )                      ->
    Tile = get_tile_at(Index, Map),
    lists:member(Tile, OKTiles).


get_loop_length({StartRow, StartCol}, Map) ->
    IndexR = {StartRow, StartCol+1},
    case is_tile_ok_for_start(right, IndexR, Map) of
        true ->
            get_loop_length(IndexR, Map, left, 1);
        false ->
            IndexB = {StartRow+1, StartCol},
            case is_tile_ok_for_start(bottom, IndexB, Map) of
                true  -> get_loop_length(IndexB,                 Map, top,   1);
                false -> get_loop_length({StartRow, StartCol-1}, Map, right, 1)
            end
    end.
    
get_loop_length({Row, Col}, Map, From, AccLength) ->
    Direction = case {From, get_tile_at({Row, Col}, Map)} of
        {_,      $S} -> finish;
        {left,   $J} -> top;
        {left,   $-} -> right;
        {left,   $7} -> bottom;
        {top,    $J} -> left;
        {top,    $|} -> bottom;
        {top,    $L} -> right;
        {right,  $L} -> top;
        {right,  $-} -> left;
        {right,  $F} -> bottom;
        {bottom, $7} -> left;
        {bottom, $|} -> top;
        {bottom, $F} -> right
    end,
    case Direction of
        finish -> AccLength;
        top    -> get_loop_length({Row-1, Col}, Map, bottom, AccLength+1);
        right  -> get_loop_length({Row, Col+1}, Map, left,   AccLength+1);
        bottom -> get_loop_length({Row+1, Col}, Map, top,    AccLength+1);
        left   -> get_loop_length({Row, Col-1}, Map, right,  AccLength+1)
    end.
    

get_tile_at({Row, Col}, Map) -> lists:nth(Col, lists:nth(Row, Map)).


get_furtherst(L) -> L div 2.
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

