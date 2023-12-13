-module(day10_2).
-export([solve/1]).

% Privertė pamąstyti. Perskaitęs užduotį pamaniau - neįmanoma! Tada pradėjau galvoti, sumąsčiau algoritmą, bet jis buvo
% per daug paprastas ir neteisingas. Pamąsčiau dar, sumąsčiąu gerą algoritmą, bet jau neturėjau laiko toliau sėdėti.
% Tai buvo maždaug pusiaukelė. Po pertraukos vėl prisėdau, suprogramavau ir viskas veikė. Buvau trečias savo leader boarde
% tą išsprendęs (https://adventofcode.com/2023/leaderboard/private/view/438010?order=local_score).

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
    Start = get_start_tile_index(Map),
    %io:fwrite("XXX START ~p~n", [Start]),
    NewMap = get_new_map(Start, Map),
    Tile = get_start_tile(Start, Map),
    {$S, MapNoS} = replace_tile_at(Start, Tile, Map),
    MapClean = clean_map(MapNoS, NewMap),
    %print_map(MapClean),
    Encircled = count_encircled(MapClean),
    Encircled.
    
    
get_map_line(Line) ->
    trim_ending_newline(Line).


get_start_tile_index(Map) ->
    get_start_tile_index({1, 1}, Map).
    
get_start_tile_index(Current,    [[$S|_       ]|_        ]) -> Current;
get_start_tile_index({Row, Col}, [[_|OtherCols]|OtherRows]) -> get_start_tile_index({Row, Col+1}, [OtherCols | OtherRows]);
get_start_tile_index({Row, _  }, [[]           |OtherRows]) -> get_start_tile_index({Row+1, 1  }, OtherRows).


get_start_tile({StartRow, StartCol}, Map) ->
    OKR = is_tile_ok_for_start(right,  {StartRow, StartCol+1}, Map),
    OKB = is_tile_ok_for_start(bottom, {StartRow+1, StartCol}, Map),
    OKL = is_tile_ok_for_start(left,   {StartRow, StartCol-1}, Map),
    OKT = is_tile_ok_for_start(top,    {StartRow-1, StartCol}, Map),
    case {OKR, OKB, OKL, OKT} of
        {true, true, false, false} -> $F;
        {true, false, true, false} -> $-;
        {true, false, false, true} -> $L;
        {false, true, true, false} -> $7;
        {false, true, false, true} -> $|;
        {false, false, true, true} -> $J
    end.


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


get_new_map({StartRow, StartCol}, Map) ->
    IndexR = {StartRow, StartCol+1},
    case is_tile_ok_for_start(right, IndexR, Map) of
        true ->
            get_new_map(IndexR, Map, left);
        false ->
            IndexB = {StartRow+1, StartCol},
            case is_tile_ok_for_start(bottom, IndexB, Map) of
                true  -> get_new_map(IndexB,                 Map, top  );
                false -> get_new_map({StartRow, StartCol-1}, Map, right)
            end
    end.
    
get_new_map({Row, Col}, Map, From) ->
    %io:fwrite("XXX ~p ~p~n", [Row, Col]),
    {TileAt, NewMap} = replace_tile_at({Row, Col}, $+, Map),
    %print_map(NewMap),
    %io:fwrite("XXX -------------~n"),
    Direction = case {From, TileAt} of
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
        finish -> NewMap;
        top    -> get_new_map({Row-1, Col}, NewMap, bottom);
        right  -> get_new_map({Row, Col+1}, NewMap, left  );
        bottom -> get_new_map({Row+1, Col}, NewMap, top   );
        left   -> get_new_map({Row, Col-1}, NewMap, right )
    end.
    
    
%print_map([]) -> ok;
%print_map([Row|OtherRows]) ->
%    io:fwrite("MAP ~p~n", [Row]),
%    print_map(OtherRows).
    
    
get_tile_at({Row, Col}, Map) -> lists:nth(Col, lists:nth(Row, Map)).


replace_tile_at({1, ColI}, With, [Row|OtherRows]) ->
    {Tile, NewRow} = replace_tile_at_col(ColI, With, Row),
    {Tile, [NewRow | OtherRows]};

replace_tile_at({RowI, ColI}, With, [Row|OtherRows]) ->
    {Tile, NewOtherRows} = replace_tile_at({RowI-1, ColI}, With, OtherRows),
    {Tile, [Row | NewOtherRows]}.
    
replace_tile_at_col(1, With, [Tile|OtherCols]) ->
    {Tile, [With|OtherCols]};
    
replace_tile_at_col(ColI, With, [Tile|OtherCols]) -> 
    {RTile, NewOtherCols} = replace_tile_at_col(ColI-1, With, OtherCols),
    {RTile, [Tile|NewOtherCols]}.


clean_map([], []) -> [];

clean_map([RowM | OtherRowsM], [RowC | OtherRowsC]) ->
    RowClean = clean_map_row(RowM, RowC),
    OtherRowsClean = clean_map(OtherRowsM, OtherRowsC),
    [RowClean | OtherRowsClean].
    
clean_map_row([], []) -> [];

clean_map_row([TileM | OtherColsM], [TileC | OtherColsC]) ->
    TileClean = clean_tile(TileM, TileC),
    OtherColsClean = clean_map_row(OtherColsM, OtherColsC),
    [TileClean | OtherColsClean].


clean_tile(Tile, $+) -> Tile;
clean_tile(_,    _ ) -> $..

count_encircled(Map) ->
    lists:foldl(fun(Row, AccCounts) ->
        %io:fwrite("XXX ~s~n", [Row]),
        {out, undefined, Count} = lists:foldl(fun
            ($., {in,    undefined, AccCount}) -> {in,                   undefined, AccCount+1};
            ($., {out,   undefined, AccCount}) -> {out,                  undefined, AccCount};
            ($|, {InOut, undefined, AccCount}) -> {invert_in_out(InOut), undefined, AccCount};
            ($L, {InOut, undefined, AccCount}) -> {InOut,                $L,        AccCount};
            ($F, {InOut, undefined, AccCount}) -> {InOut,                $F,        AccCount};
            ($-, {InOut, Start,     AccCount}) -> {InOut,                Start,     AccCount};
            ($J, {InOut, $F,        AccCount}) -> {invert_in_out(InOut), undefined, AccCount};
            ($J, {InOut, $L,        AccCount}) -> {InOut,                undefined, AccCount};
            ($7, {InOut, $F,        AccCount}) -> {InOut,                undefined, AccCount};
            ($7, {InOut, $L,        AccCount}) -> {invert_in_out(InOut), undefined, AccCount}
        end, {out, undefined, 0}, Row),
        Count + AccCounts
    end, 0, Map).
    
    
invert_in_out(in ) -> out;
invert_in_out(out) -> in.
    

trim_ending_newline(Str) ->
    string:sub_string(Str, 1, string:len(Str) - 1).

